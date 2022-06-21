{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Api.Image where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ask)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Data (Proxy (..), Typeable)
import Data.String (IsString (fromString))
import Handlers.App (App, Handler (Handler), hImageHandler)
import Handlers.DB.Scheme (ImageId)
import qualified Handlers.Image as I
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
import Servant.API.ContentTypes (AllCTRender (handleAcceptH), MimeUnrender (mimeUnrender))

data WithCT = WithCT {header :: BS.ByteString, content :: BS.ByteString}

instance AllCTRender '[IMAGE] WithCT where
  handleAcceptH _ _ (WithCT h c) = Just (LBS.fromStrict h, LBS.fromStrict c)

data IMAGE deriving (Typeable)

instance MimeRender IMAGE BS.ByteString where
  mimeRender _ = LBS.fromStrict

instance MimeUnrender IMAGE WithCT where
  mimeUnrender _ bs = Right $ WithCT "" (LBS.toStrict bs)

instance Accept IMAGE where
  contentType _ = "image/*"

type ImageApi = Capture "image_id" ImageId :> Get '[IMAGE] WithCT

imageApi :: Proxy ImageApi
imageApi = Proxy

imageServer :: (MonadCatch imageM, MonadIO imageM) => ServerT ImageApi (App imageM)
imageServer = getI

getI :: (MonadCatch imageM, MonadIO imageM) => ImageId -> App imageM WithCT
getI imId = do
  Handler {..} <- ask
  imageData <- I.runImage hImageHandler $ I.getImageData imId
  case imageData of
    Nothing -> throwError $ err400 {errReasonPhrase = "No image with provided id."}
    Just (contentType_, imageBytes) -> return $ WithCT (fromString contentType_) imageBytes