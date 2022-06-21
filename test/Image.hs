{-# LANGUAGE OverloadedStrings #-}

module Image where

import Api.Image (WithCT (WithCT), imageApi, imageServer)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as LBS
import Data.String (IsString (fromString))
import qualified Data.Text as T
import qualified Database.Persist as P
import qualified Database.Persist.Sql as P
import qualified Handlers.App as A
import qualified Handlers.DB as DB
import Handlers.DB.Scheme
import Handlers.Image
import Image.Test (ImageTestIO (unImageTestIO))
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp
import Servant.Client
import Servant.Multipart (FileData (FileData, fdFileCType, fdPayload), Mem)
import Test.Hspec
import Utils (clearImages, respondsWithErr, shouldBeRightOr, withApp)

getI :: ImageId -> ClientM WithCT
getI = client imageApi

imageSpec :: A.Handler ImageTestIO -> Spec
imageSpec h = before (clearImages h) $
  around (withApp h imageApi imageServer) $ do
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl {baseUrlPort = port})

    imageGetI h clientEnv

imageGetI :: A.Handler ImageTestIO -> (Port -> ClientEnv) -> SpecWith Port
imageGetI h clientEnv =
  describe "GET /image" $ do
    context "existing image id" $ do
      it "returns an image" $ \port -> do
        let fd = FileData "someImName" "" "image/jpeg" "imContent" :: FileData Mem
        imFp <- liftIO . unImageTestIO $ do
          env <- hPrepareEnv (A.hImageHandler h)
          hPutImage (A.hImageHandler h) env fd
        imId <- DB.hRunDB (A.hDBHandler h) $ P.insert $ Image (T.unpack . fdFileCType $ fd) imFp
        WithCT _ imReturned <-
          runClientM (getI imId) (clientEnv port)
            >>= shouldBeRightOr "Internal server or client error"
        imReturned `shouldBe` (LBS.toStrict . fdPayload $ fd)

    context "not existing image" $ do
      it "responds with 400" $ \port -> do
        runClientM (getI (ImageKey . P.SqlBackendKey $ 1)) (clientEnv port)
          >>= respondsWithErr 400
