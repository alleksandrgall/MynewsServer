{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Api.Image where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import Data.ByteString.Lazy (fromStrict)
import Data.Data (Typeable)
import Data.String (IsString (fromString))
import qualified Database.Persist.Sql as P
import Handlers.App (App, runDB)
import Handlers.DB.Scheme (Image (imageMime, imagePath), ImageId)
import Servant
  ( Accept (contentType),
    Capture,
    Get,
    HasServer (ServerT),
    MimeRender (..),
    ServerError (errReasonPhrase),
    err400,
    throwError,
    type (:>),
  )
import Servant.API.ContentTypes (AllCTRender (handleAcceptH))

data WithCT = WithCT {header :: BS.ByteString, content :: BS.ByteString}

instance AllCTRender '[IMAGE] WithCT where
  handleAcceptH _ _ (WithCT h c) = Just (fromStrict h, fromStrict c)

data IMAGE deriving (Typeable)

instance MimeRender IMAGE BS.ByteString where
  mimeRender _ = fromStrict

instance Accept IMAGE where
  contentType _ = ""

type ImageApi = Capture "image_id" ImageId :> Get '[IMAGE] WithCT

imageServer :: ServerT ImageApi App
imageServer = getI'

getI' :: ImageId -> App WithCT
getI' imId = do
  maybeImage <- runDB $ P.get imId
  case maybeImage of
    Nothing -> throwError err400 {errReasonPhrase = "No such image"}
    Just image -> do
      let contentTypeHeader = imageMime image
      imageBytes <- liftIO $ BS.readFile (imagePath image)
      return $ WithCT (fromString contentTypeHeader) $ Base64.encode imageBytes