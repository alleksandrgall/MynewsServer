{-# LANGUAGE OverloadedStrings #-}

module Utils
  ( validateImages,
    saveInsertToDbImages,
  )
where

import Control.Exception (onException)
import Control.Monad (foldM, void, when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import DB.Scheme (EntityField (ImageMime, ImagePath), Image (Image, imagePath), ImageId, Key (unImageKey))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Time (UTCTime (utctDay), getCurrentTime, toGregorian)
import Database.Persist.Sql
import qualified Database.Persist.Sql as P
import Dev
import Servant
import Servant.Multipart
import System.Directory (createDirectoryIfMissing, removeFile)

newtype Valid = Valid [FileData Mem]

validateImages :: [FileData Mem] -> Handler Valid
validateImages fds = do
  when (length fds > maxFilesDev) (throwError err413 {errReasonPhrase = "Too many files, max file num is " ++ show maxFilesDev})
  mapM_
    ( \fd ->
        when (T.takeWhile (/= '/') (fdFileCType fd) /= "image") (throwError err415)
          >> when (LBS.length (fdPayload fd) > imageSizeDev) (throwError err413) --not an image
    )
    fds --file too large
  return $ Valid fds

saveInsertToDbImages :: Valid -> String -> (ImageId -> SqlPersistM ()) -> IO [ImageId]
saveInsertToDbImages (Valid fds) imageRootDev inserter = do
  (year, month', day') <- toGregorian . utctDay <$> getCurrentTime
  let trg = imageRootDev ++ "/" ++ show year ++ "/" ++ show month' ++ "/" ++ show day'
  createDirectoryIfMissing True trg
  let insertDB =
        foldM
          ( \prev fd -> do
              [im] <- rawSqlInsertFileWithId trg fd
              inserter (entityKey im)
              return ((entityKey im, imagePath . entityVal $ im, fdPayload fd) : prev)
          )
          []
          fds
  runDBDev $ do
    fls <- insertDB
    liftIO $ onException (mapM (\(_, fp, bs) -> LBS.writeFile fp bs) fls) (mapM (\(_, fp, _) -> removeFile fp) fls)
    return $ map (\(imId, _, _) -> imId) fls

rawSqlInsertFileWithId :: (MonadIO m) => FilePath -> FileData Mem -> SqlPersistT m [Entity Image]
rawSqlInsertFileWithId root fd =
  P.rawSql
    " INSERT INTO image (path, mime) VALUES (? || currval('image_image_id_seq')|| ?, ?) RETURNING "
    [ P.PersistText $ T.pack root <> "/" <> "image",
      P.PersistText $ T.dropWhile (/= '.') (fdFileName fd),
      P.PersistText $ fdFileCType fd
    ]
