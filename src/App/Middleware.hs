{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module App.Middleware where

import App.App (AppConfig (logConfig), LogConfig (logContext, logEnv, logNamespace))
import Control.Monad.Reader (MonadIO (..), MonadReader (ask, local), ReaderT (..), asks)
import Data.Aeson ((.=))
import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import qualified Katip as K
import Network.HTTP.Types (HttpVersion, Method, Query, Status, queryToQueryText)
import qualified Network.Wai as W

newtype KatipM a = KatipM {unKatipM :: ReaderT LogConfig IO a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader LogConfig,
      MonadIO
    )

instance K.Katip KatipM where
  getLogEnv = asks logEnv
  localLogEnv f (KatipM m) = KatipM $ local (\s -> s {logEnv = f $ logEnv s}) m

instance K.KatipContext KatipM where
  getKatipContext = asks logContext
  localKatipContext f (KatipM m) = KatipM (local (\s -> s {logContext = f $ logContext s}) m)
  getKatipNamespace = asks logNamespace
  localKatipNamespace f (KatipM m) = KatipM (local (\s -> s {logNamespace = f $ logNamespace s}) m)

type ApplicationK = W.Request -> (W.Response -> KatipM W.ResponseReceived) -> KatipM W.ResponseReceived

type MiddlewareK = ApplicationK -> ApplicationK

runApplicationK :: LogConfig -> ApplicationK -> W.Application
runApplicationK appConf app req respReceived = flip runReaderT appConf . unKatipM $ app req (liftIO . respReceived)

data Request = Request
  { requestHttpVersion :: HttpVersion,
    requestIsSecure :: Bool,
    requestMethod :: Method,
    requestPathInfo :: [T.Text],
    requestQueryString :: Query,
    requestBodyLength :: W.RequestBodyLength,
    requestHeaderHost :: Maybe ByteString,
    requestHeaderReferer :: Maybe ByteString,
    requestHeaderUserAgent :: Maybe ByteString,
    requestHeaderRange :: Maybe ByteString
  }
  deriving (Show, Generic)

instance A.ToJSON Request where
  toJSON Request {..} =
    let headers =
          A.object
            [ "host" .= fmap decodeUtf8 requestHeaderHost,
              "referer" .= fmap decodeUtf8 requestHeaderReferer,
              "userAgent" .= fmap decodeUtf8 requestHeaderUserAgent,
              "range" .= fmap decodeUtf8 requestHeaderRange
            ]
     in A.object
          [ "httpVersion" .= show requestHttpVersion,
            "isSecure" .= requestIsSecure,
            "method" .= decodeUtf8 requestMethod,
            "path" .= T.intercalate "/" requestPathInfo,
            "queryString" .= queryToQueryText requestQueryString,
            "bodyLength" .= show requestBodyLength,
            "headers" .= headers
          ]

formatRequest :: W.Request -> A.Value
formatRequest req =
  A.toJSON $
    Request
      { requestHttpVersion = W.httpVersion req,
        requestIsSecure = W.isSecure req,
        requestMethod = W.requestMethod req,
        requestPathInfo = W.pathInfo req,
        requestQueryString = W.queryString req,
        requestBodyLength = W.requestBodyLength req,
        requestHeaderHost = W.requestHeaderHost req,
        requestHeaderReferer = W.requestHeaderReferer req,
        requestHeaderUserAgent = W.requestHeaderUserAgent req,
        requestHeaderRange = W.requestHeaderRange req
      }

newtype Response = Response {responseStatus :: Status} deriving (Show)

instance A.ToJSON Response where
  toJSON Response {..} = A.object ["status" .= show responseStatus]

formatResponse :: W.Response -> A.Value
formatResponse resp = A.toJSON $ Response (W.responseStatus resp)

katipMiddleware :: K.Severity -> MiddlewareK
katipMiddleware sev baseApp req respRecieved =
  K.katipAddContext (K.sl "request" (A.toJSON . formatRequest $ req)) $ do
    K.logFM sev "Request received"
    baseApp req $ \resp ->
      K.katipAddContext (K.sl "response" (A.toJSON . formatResponse $ resp)) $ do
        K.logFM sev "Reponse sent"
        respRecieved resp

mkApplicationK :: (AppConfig -> W.Application) -> (AppConfig -> ApplicationK)
mkApplicationK f appConfig req respReceived = do
  localLogConfig <- ask
  liftIO $ f (appConfig {logConfig = localLogConfig}) req (flip runReaderT localLogConfig . unKatipM . respReceived)
