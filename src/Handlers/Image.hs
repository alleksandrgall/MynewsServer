{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Handlers.Image where

import Control.Monad (foldM, when, (>=>))
import Control.Monad.Catch (Exception, MonadCatch (catch), MonadMask, MonadThrow (throwM), SomeException, bracketOnError, handle, onException)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (Options (fieldLabelModifier), ToJSON, camelTo2, defaultOptions, genericToJSON)
import Data.Aeson.Types (ToJSON (toJSON))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (isNothing)
import qualified Data.Text as T
import qualified Database.Persist as P
import qualified Database.Persist.Sql as P
import GHC.Generics (Generic)
import qualified Handlers.DB as DB
import Handlers.DB.Scheme (EntityField (ImageId), Image (Image, imagePath), ImageId)
import Katip (Katip, KatipContext)
import qualified Katip as K
import Servant.Multipart (FileData (fdFileCType, fdPayload), Mem)
import System.IO.Error (isDoesNotExistError)

data Config m = Config
  { cMaxImageSize :: m Int64,
    cMaxNumberOfImages :: m Int
  }

data Handler m = Handler
  { hConfig :: Config m,
    hDBHandler :: DB.Handler,
    hPrepareEnv :: m (ImageEnv m),
    hPutImage :: ImageEnv m -> FileData Mem -> m FilePath,
    hGetImage :: FilePath -> m BS.ByteString,
    hDeleteImage :: FilePath -> m ()
  }

newtype FileExt = FileExt {unFileExt :: String}

data ImageException
  = ImageTooLargeException
  | TooManyImagesException
  | NotAnImageException
  | PutException (FileData Mem) SomeException
  | GetException Image SomeException
  | DeleteException [ImageId] (P.Entity Image) SomeException
  deriving (Show)

instance Exception ImageException

type family ImageEnv (m :: * -> *)

saveImages :: (MonadMask m, MonadIO m) => Handler m -> (ImageId -> P.SqlPersistM ()) -> NonEmpty (FileData Mem) -> m (NonEmpty ImageId)
saveImages Handler {..} custInserter fdImageS@(fdImageHead :| fdImageRest) = do
  maxFiles <- cMaxNumberOfImages hConfig
  maxImSize <- cMaxImageSize hConfig
  when (length fdImageS > maxFiles) $ throwM TooManyImagesException
  mapM_
    ( \fdIm ->
        when (LBS.length (fdPayload fdIm) > maxImSize) (throwM ImageTooLargeException)
          >> when (T.takeWhile (/= '/') (fdFileCType fdIm) /= "image") (throwM NotAnImageException)
    )
    fdImageS
  imageEnv <- hPrepareEnv
  headImage <- putImageCatch imageEnv fdImageHead
  restImages <- foldM (\acc fdImage -> putImageCatch imageEnv fdImage >>= \im -> return (im : acc) `onException` clearImages acc) [] fdImageS
  liftIO (DB.hRunDB hDBHandler $ mapM (P.insert >=> (\imId -> custInserter imId >> return imId)) (headImage :| restImages))
    `onException` clearImages (headImage : restImages)
  where
    putImageCatch imageEnv fdImage =
      hPutImage imageEnv fdImage `catch` (throwM . PutException fdImage)
        <&> Image (T.unpack $ fdFileCType fdImageHead)
    clearImages = mapM_ (\(Image _ fp) -> hDeleteImage fp)

getImageData :: (MonadCatch m, MonadIO m) => Handler m -> ImageId -> m (Maybe (String, BS.ByteString))
getImageData Handler {..} imId = do
  maybeImage <- liftIO $ DB.hRunDB hDBHandler $ P.get imId
  case maybeImage of
    Nothing -> return Nothing
    Just im@(Image mime path) -> Just . (mime,) <$> hGetImage path `catch` (throwM . GetException im)

data DeleteStatus = DeleteStatus
  { deleteStatus :: Bool,
    deleted :: [ImageId]
  }
  deriving (Show, Generic)

instance ToJSON DeleteStatus where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

deleteImages :: (MonadCatch m, MonadIO m, Katip m, KatipContext m) => Handler m -> [ImageId] -> m [ImageId]
deleteImages Handler {..} imagesIds = do
  images <- liftIO $ DB.hRunDB hDBHandler $ P.selectList [ImageId P.<-. imagesIds] []
  foldM actuallyDeleteImages [] images
  where
    handleDeleteException (DeleteException ims imageEnt se) =
      K.katipAddContext (K.sl "deleted_images" ims <> K.sl "deleting_error" (show se) <> K.sl "problem_image" (show imageEnt)) $ do
        K.logFM K.ErrorS "Not all of the requested images where deleted."
        return $ DeleteStatus False ims
    handleDeleteException e = throwM e
    actuallyDeleteImages ims imageEnt = (actuallyDeleteImage imageEnt >>= \imD -> return $ imD : ims) `catch` (throwM . DeleteException ims imageEnt)
    actuallyDeleteImage imEnt = do
      hDeleteImage (imagePath . P.entityVal $ imEnt) `catch` handleDoesNotExistError
      liftIO $ DB.hRunDB hDBHandler $ P.delete (P.entityKey imEnt)
      return (P.entityKey imEnt)
    handleDoesNotExistError e
      | True <- isDoesNotExistError e = return ()
      | otherwise = throwM e