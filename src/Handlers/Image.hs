{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Handlers.Image where

import Control.Monad (foldM, when)
import Control.Monad.Catch (Exception, MonadCatch (catch), MonadThrow (throwM), SomeException, handle)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text, pack)
import qualified Handlers.DB as DB
import Servant.Multipart (FileData (fdPayload), Mem)

newtype FileExt = FileExt {unFileExt :: String}

data ImageException
  = ImageTooLargeException
  | TooManyImagesException
  | PutException (FileData Mem) SomeException
  | GetException Text SomeException
  | DeleteException (FileData Mem) SomeException
  deriving (Show)

instance Exception ImageException

type family ImageEnv (m :: * -> *)

data Config m = Config
  { cMaxImageSize :: m Int64,
    cMaxNumberOfImages :: m Int
  }

data Handler m = Handler
  { hConfig :: Config m,
    hDBHandler :: DB.Handler,
    hIsImages :: NonEmpty (FileData Mem) -> m (),
    hPrepareEnv :: m (ImageEnv m),
    hPutImage :: ImageEnv m -> FileData Mem -> m FilePath,
    hGetImage :: FilePath -> m BS.ByteString,
    hDeleteImage :: FilePath -> m ()
  }

saveImages :: (MonadCatch m) => Handler m -> NonEmpty (FileData Mem) -> m (NonEmpty FilePath)
saveImages Handler {..} fdImageS@(fdImage :| fdImageRest) = do
  maxFiles <- cMaxNumberOfImages hConfig
  maxImSize <- cMaxImageSize hConfig
  when (length fdImageS > maxFiles) $ throwM TooManyImagesException
  mapM_ (\fdIm -> when (LBS.length (fdPayload fdIm) > maxImSize) $ throwM ImageTooLargeException) fdImageS
  hIsImages fdImageS
  imageEnv <- hPrepareEnv
  headLoc <- hPutImage imageEnv fdImage `catch` (throwM . PutException fdImage)
  restLoc <-
    foldM
      ( \acc fdImage ->
          hPutImage imageEnv fdImage
            `catch` (mapM throwM . PutException fdImage) <&> flip (:) acc
      )
      []
      fdImageS
  return (headLoc :| restLoc)

getImageData :: (MonadCatch m) => Handler m -> FilePath -> m BS.ByteString
getImageData Handler {..} imLoc = hGetImage imLoc `catch` (throwM . GetException (pack . show $ imLoc))

-- let insertOneDB = do
--       (imageFp, h) <- liftIO $ mkstemps trg (takeExtension (T.unpack $ fdFileName f))
--       imId <- insertImage imageFp f
--       inserter imId
--       return (h, imageFp, fdPayload f, imId)
--     insertDB =
--       foldM
--         ( \prev fd -> do
--             (imageFp, h) <- liftIO $ mkstemps trg (takeExtension (T.unpack $ fdFileName f))
--             imId <- insertImage imageFp f
--             inserter imId
--             return ((h, imageFp, fdPayload fd, imId) : prev)
--         )
--         []
--         fds
-- runDB $ do
--   imageData <- insertOneDB
--   imageDataS <- insertDB
--   liftIO $ do
--     onException
--       (mapM_ (\(h, _, bs, _) -> LBS.hPut h bs) (imageData :| imageDataS))
--       (mapM_ (\(h, fp, _, _) -> hClose h >> removeFile fp) (imageData :| imageDataS))
--     mapM_ (\(h, _, _, _) -> hClose h) (imageData :| imageDataS)
--   return $ fmap (\(_, _, _, imId) -> imId) (imageData :| imageDataS)