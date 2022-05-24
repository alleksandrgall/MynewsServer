{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module KatipMiddleware
  ( katipMiddleware,
  )
where

import App
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader (ask, local), ReaderT (runReaderT), asks)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Text as T (Text, intercalate)
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import Katip
import Network.HTTP.Types
import qualified Network.Wai as W

newtype KatipM a = KatipM {unKatipM :: ReaderT LogConfig IO a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader LogConfig,
      MonadIO
    )

instance Katip KatipM where
  getLogEnv = asks logEnv
  localLogEnv f (KatipM m) = KatipM $ local (\s -> s {logEnv = f $ logEnv s}) m

instance KatipContext KatipM where
  getKatipContext = asks logContext
  localKatipContext f (KatipM m) = KatipM (local (\s -> s {logContext = f $ logContext s}) m)
  getKatipNamespace = asks logNamespace
  localKatipNamespace f (KatipM m) = KatipM (local (\s -> s {logNamespace = f $ logNamespace s}) m)

type ApplicationK = W.Request -> (W.Response -> KatipM W.ResponseReceived) -> KatipM W.ResponseReceived

type MiddlewareK = W.Application -> ApplicationK

data Request = Request
  { requestHttpVersion :: HttpVersion,
    requestIsSecure :: Bool,
    requestMethod :: Method,
    requestPathInfo :: [Text],
    requestQueryString :: Query,
    requestBodyLength :: W.RequestBodyLength,
    requestHeaderHost :: Maybe ByteString,
    requestHeaderReferer :: Maybe ByteString,
    requestHeaderUserAgent :: Maybe ByteString,
    requestHeaderRange :: Maybe ByteString
  }
  deriving (Show, Generic)

instance ToJSON Request where
  toJSON Request {..} =
    let headers =
          object
            [ "host" .= fmap decodeUtf8 requestHeaderHost,
              "referer" .= fmap decodeUtf8 requestHeaderReferer,
              "userAgent" .= fmap decodeUtf8 requestHeaderUserAgent,
              "range" .= fmap decodeUtf8 requestHeaderRange
            ]
     in object
          [ "httpVersion" .= show requestHttpVersion,
            "isSecure" .= requestIsSecure,
            "method" .= decodeUtf8 requestMethod,
            "path" .= T.intercalate "/" requestPathInfo,
            "queryString" .= queryToQueryText requestQueryString,
            "bodyLength" .= show requestBodyLength,
            "headers" .= headers
          ]

formatRequest :: W.Request -> Value
formatRequest req =
  toJSON $
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

instance ToJSON Response where
  toJSON Response {..} = object ["status" .= show responseStatus]

formatResponse :: W.Response -> Value
formatResponse resp = toJSON $ Response (W.responseStatus resp)

katipMiddlewareInternal :: Severity -> MiddlewareK
katipMiddlewareInternal sev baseApp req respRecieved =
  katipAddContext (sl "request" (toJSON . formatRequest $ req)) $ do
    logFM sev "Request received"
    logConf <- ask
    liftIO $
      baseApp req $ \resp -> flip runReaderT logConf . unKatipM $
        katipAddContext (sl "response" (toJSON . formatResponse $ resp)) $ do
          logFM sev "Reponse sent"
          respRecieved resp

runApplicationK :: LogConfig -> ApplicationK -> W.Application
runApplicationK logConf appK req respRecieved = runReaderT (unKatipM $ appK req (liftIO . respRecieved)) logConf

katipMiddleware :: LogConfig -> Severity -> W.Middleware
katipMiddleware logConf sev baseApp = runApplicationK logConf (katipMiddlewareInternal sev baseApp)