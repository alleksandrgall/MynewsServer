module Handlers.App.HandleExcept where

import Control.Monad.Catch (MonadThrow (throwM), handle)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Data.String (IsString (fromString))
import qualified Data.Text as T
import Handlers.App (App, Handler (hImageHandler))
import qualified Handlers.Image as I
import qualified Katip as K
import Servant (ServerError (errReasonPhrase), err413, err415, err500, throwError)
import Servant.Multipart (FileData (fdFileName, fdInputName))

handlePutException :: App imageM a -> App imageM a
handlePutException = handle $ \case
  (I.PutException fd se) -> do
    K.logFM K.ErrorS $ "Error while inputing an image: " <> (fromString . show $ se)
    throwError err500 {errReasonPhrase = "Wasn't able to save an image: " <> (T.unpack . fdInputName $ fd)}
  e -> throwM e

handleFormatExcept :: (Monad imageM) => App imageM a -> App imageM a
handleFormatExcept = handle $ \case
  (I.ImageTooLargeException fd) -> do
    imH <- asks hImageHandler
    maxImSize <- I.runImage imH (lift $ I.cMaxImageSize . I.hConfig $ imH)
    throwError err413 {errReasonPhrase = "File is too large, file name: " ++ (T.unpack . fdFileName $ fd) ++ ", max size: " ++ show maxImSize}
  I.TooManyImagesException -> do
    imH <- asks hImageHandler
    maxFiles <- I.runImage imH (lift $ I.cMaxNumberOfImages . I.hConfig $ imH)
    throwError err413 {errReasonPhrase = "Too many files, max file num is " ++ show maxFiles}
  (I.NotAnImageException fd) -> throwError err415 {errReasonPhrase = "Not an image, file name: " ++ (T.unpack . fdFileName $ fd)}
  e -> throwM e

handleDeleteException :: App imageM I.DeleteStatus -> App imageM I.DeleteStatus
handleDeleteException = handle $ \case
  (I.DeleteException ims imageEnt se) -> do
    K.katipAddContext (K.sl "deleted_images" ims <> K.sl "deleting_error" (show se) <> K.sl "problem_image" (show imageEnt)) $ do
      K.logFM K.ErrorS "Not all of the requested images where deleted."
      return $ I.DeleteStatus False ims
  e -> throwM e

handleGetException :: App imageM a -> App imageM a
handleGetException = handle $ \case
  (I.GetException imageEnt se) -> do
    K.katipAddContext (K.sl "deleting_error" (show se) <> K.sl "problem_image" (show imageEnt)) $ do
      K.logFM K.ErrorS "Image was found on the DB but not in image storage"
      throwError err500 {errReasonPhrase = "Internal storage error"}
  e -> throwM e
