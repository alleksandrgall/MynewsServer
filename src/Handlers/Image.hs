{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Handlers.Image where

import Control.Monad (foldM, when, (>=>))
import Control.Monad.Catch (Exception, MonadCatch (catch), MonadMask, MonadThrow (throwM), SomeException, onException)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Control.Monad.Trans (lift)
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text as T
import qualified Database.Persist as P
import qualified Database.Persist.Sql as P
import GHC.Generics (Generic)
import qualified Handlers.DB as DB
import Handlers.DB.Scheme (EntityField (ImageId), Image (Image, imagePath), ImageId)
import Servant.Multipart (FileData (fdFileCType, fdPayload), Mem)

data Config m = Config
  { cMaxImageSize :: m Int64,
    cMaxNumberOfImages :: m Int
  }

data Handler m = Handler
  { hConfig :: Config m,
    hRunImage :: forall a. m a -> IO a,
    hDBHandler :: DB.Handler,
    hPrepareEnv :: m (ImageEnv m),
    hPutImage :: ImageEnv m -> FileData Mem -> m FilePath,
    hGetImage :: FilePath -> m BS.ByteString,
    hDeleteImage :: FilePath -> m ()
  }

type family ImageEnv (m :: * -> *)

newtype FileExt = FileExt {unFileExt :: String}

data ImageException
  = ImageTooLargeException (FileData Mem)
  | TooManyImagesException
  | NotAnImageException (FileData Mem)
  | PutException (FileData Mem) SomeException
  | GetException (P.Entity Image) SomeException
  | DeleteException [ImageId] (P.Entity Image) SomeException
  deriving (Show)

instance Exception ImageException

runImage :: (MonadIO n) => Handler m -> ReaderT (Handler m) m a -> n a
runImage h m = liftIO $ hRunImage h $ runReaderT m h

saveImages :: (MonadMask m, MonadIO m) => (ImageId -> P.SqlPersistM ()) -> NonEmpty (FileData Mem) -> ReaderT (Handler m) m (NonEmpty ImageId)
saveImages custInserter fdImageS@(fdImageHead :| fdImageRest) = do
  h@Handler {..} <- ask
  maxFiles <- lift $ cMaxNumberOfImages hConfig
  maxImSize <- lift $ cMaxImageSize hConfig
  when (length fdImageS > maxFiles) $ throwM TooManyImagesException
  mapM_
    ( \fdIm ->
        when (LBS.length (fdPayload fdIm) > maxImSize) (throwM $ ImageTooLargeException fdIm)
          >> when (T.takeWhile (/= '/') (fdFileCType fdIm) /= "image") (throwM $ NotAnImageException fdIm)
    )
    fdImageS
  imageEnv <- lift hPrepareEnv
  headImage <- putImageCatch h imageEnv fdImageHead
  restImages <- foldM (\acc fdImage -> putImageCatch h imageEnv fdImage >>= \im -> return (im : acc) `onException` clearImages h acc) [] fdImageRest
  liftIO (DB.hRunDB hDBHandler $ mapM (P.insert >=> (\imId -> custInserter imId >> return imId)) (headImage :| restImages))
    `onException` clearImages h (headImage : restImages)
  where
    putImageCatch Handler {..} imageEnv fdImage =
      lift $
        hPutImage imageEnv fdImage `catch` (throwM . PutException fdImage)
          <&> Image (T.unpack $ fdFileCType fdImage)
    clearImages Handler {..} images = lift $ mapM_ (\(Image _ fp) -> hDeleteImage fp) images

getImageData :: (MonadCatch m, MonadIO m) => ImageId -> ReaderT (Handler m) m (Maybe (String, BS.ByteString))
getImageData imId = do
  Handler {..} <- ask
  maybeImage <- liftIO $ DB.hRunDB hDBHandler $ P.get imId
  case maybeImage of
    Nothing -> return Nothing
    Just im@(Image mime path) -> Just . (mime,) <$> lift (hGetImage path) `catch` (throwM . GetException (P.Entity imId im))

data DeleteStatus = DeleteStatus
  { deleteStatus :: Bool,
    deleted :: [ImageId]
  }
  deriving (Show, Generic)

instance A.FromJSON DeleteStatus where
  parseJSON = A.genericParseJSON A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_'}

instance A.ToJSON DeleteStatus where
  toJSON = A.genericToJSON A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_'}

deleteImages :: (MonadCatch m, MonadIO m) => [ImageId] -> ReaderT (Handler m) m DeleteStatus
deleteImages imagesIds = do
  h@Handler {..} <- ask
  images <- liftIO $ DB.hRunDB hDBHandler $ P.selectList [ImageId P.<-. imagesIds] []
  DeleteStatus True . reverse <$> foldM (actuallyDeleteImages h) [] images
  where
    actuallyDeleteImages h ims imageEnt = do (actuallyDeleteImage h imageEnt >>= \imD -> return $ imD : ims) `catch` (throwM . DeleteException ims imageEnt)
    actuallyDeleteImage Handler {..} imEnt = do
      lift $ hDeleteImage (imagePath . P.entityVal $ imEnt)
      liftIO $ DB.hRunDB hDBHandler $ P.delete (P.entityKey imEnt)
      return (P.entityKey imEnt)