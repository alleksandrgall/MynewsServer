{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Api.Image where

import Api.Internal.ImageManager
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LBS
import Data.Data (Proxy (..), Typeable)
import Data.String (IsString (fromString))
import Handlers.App (App)
import Handlers.DB.Scheme (ImageId)
import Servant
  ( Accept (contentType),
    Capture,
    Get,
    HasServer (ServerT),
    MimeRender (..),
    type (:>),
  )
import Servant.API (MimeUnrender (mimeUnrender))
import Servant.API.ContentTypes (AllCTRender (handleAcceptH), MimeUnrender)

data WithCT = WithCT {header :: BS.ByteString, content :: BS.ByteString}

instance AllCTRender '[IMAGE] WithCT where
  handleAcceptH _ _ (WithCT h c) = Just (LBS.fromStrict h, LBS.fromStrict c)

data IMAGE deriving (Typeable)

instance MimeRender IMAGE BS.ByteString where
  mimeRender _ = LBS.fromStrict

instance MimeUnrender IMAGE WithCT where
  mimeUnrender _ = Right . LBS.toStrict

instance Accept IMAGE where
  contentType _ = ""

type ImageApi = Capture "image_id" ImageId :> Get '[IMAGE] WithCT

imageApi = Proxy :: Proxy ImageApi

imageServer :: ServerT ImageApi App
imageServer = getI

getI :: ImageId -> App WithCT
getI imId = do
  (contentType_, imageBytes) <- getImage imId
  return $ WithCT (fromString contentType_) $ Base64.encode imageBytes