{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Utils
  ( validateImages,
    saveInsertToDbImages,
  )
where

import Control.Exception (onException)
import Control.Monad (foldM, void, when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import DB.Scheme (EntityField (ImageMime, ImagePath), Image (Image), ImageId, Key (unImageKey))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Time (UTCTime (utctDay), getCurrentTime, toGregorian)
import Database.Persist.Sql
import qualified Database.Persist.Sql as P
import Dev
import Servant
import Servant.Multipart
import System.Directory (createDirectoryIfMissing, removeFile)

data Valid = Valid [FileData Mem]

saveInsertToDbImages :: Valid -> String -> (ImageId -> SqlPersistM ()) -> IO [ImageId]
saveInsertToDbImages (Valid fds) imageRootDev inserter = do
  (year, month', day') <- toGregorian . utctDay <$> getCurrentTime
  let trg = imageRootDev ++ "/" ++ show year ++ "/" ++ show month' ++ "/" ++ show day'
  createDirectoryIfMissing True trg
  let insertDB =
        foldM
          ( \prev fd -> do
              imId <-
                P.insert tempImage >>= \imId ->
                  P.update
                    imId
                    [ImagePath P.=. makeFName trg imId fd, ImageMime P.=. (T.unpack . fdFileCType $ fd)]
                    >> return imId
              inserter imId
              return ((imId, makeFName trg imId fd, fdPayload fd) : prev)
          )
          []
          fds
  runDBDev $ do
    fls <- insertDB
    liftIO $ onException (mapM (\(_, fp, bs) -> LBS.writeFile fp bs) fls) (mapM (\(_, fp, _) -> removeFile fp) fls)
    return $ map (\(imId, _, _) -> imId) fls

makeFName :: FilePath -> Key Image -> FileData tag -> FilePath
makeFName trg i fd = trg ++ "/" ++ "image" ++ (show . unSqlBackendKey . unImageKey $ i) ++ dropWhile (/= '.') (T.unpack . fdFileName $ fd)

tempImage :: Image
tempImage = Image "" ""

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
