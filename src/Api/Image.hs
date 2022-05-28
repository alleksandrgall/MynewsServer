{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Api.Image where

import App
import Control.Monad.IO.Class (liftIO)
import DB.Scheme (Image (imageMime, imagePath), ImageId)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import Data.ByteString.Lazy (fromStrict)
import Data.Data (Typeable)
import Data.List.NonEmpty
import Data.String (IsString (fromString))
import qualified Database.Persist.Sql as P
import GHC.TypeLits (KnownSymbol)
import Network.HTTP.Media (MediaType, RenderHeader (renderHeader), (//))
import qualified Network.Wai as W
import Servant
import Servant.API.ContentTypes (AllCTRender (handleAcceptH), AllMime (allMime))

data WithCT = WithCT {header :: BS.ByteString, content :: BS.ByteString}

instance AllCTRender '[IMAGE] WithCT where
  handleAcceptH _ _ (WithCT h c) = Just (fromStrict h, fromStrict c)

data IMAGE deriving (Typeable)

instance MimeRender IMAGE BS.ByteString where
  mimeRender _ content = fromStrict content

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
      return $ WithCT (fromString contentTypeHeader) imageBytes

getI :: ImageId -> App (Headers '[Header "Content-Type" String] BS.ByteString)
getI imId = do
  maybeImage <- runDB $ P.get imId
  case maybeImage of
    Nothing -> throwError err400 {errReasonPhrase = "No such image"}
    Just image -> do
      let contentTypeHeader = imageMime image
      imageBytes <- liftIO $ BS.readFile (imagePath image)
      return $ addHeader contentTypeHeader $ Base64.encode imageBytes