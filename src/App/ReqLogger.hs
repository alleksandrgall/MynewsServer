{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module App.ReqLogger
  (
  )
where

import App.App
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader (ask, local), ReaderT (runReaderT), asks)
import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), Value, object)
import Data.ByteString (ByteString)
import Data.Text as T (Text, intercalate)
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import qualified Katip as K
import Network.HTTP.Types
  ( HttpVersion,
    Method,
    Query,
    Status,
    queryToQueryText,
  )
import qualified Network.Wai as W
import Servant (Handler (Handler))

type ApplicationK m = W.Request -> (W.Response -> AppT m W.ResponseReceived) -> AppT m W.ResponseReceived

type MiddlewareK m = ApplicationK m -> ApplicationK m

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

katipMiddlewareInternal :: (MonadIO m) => K.Severity -> MiddlewareK m
katipMiddlewareInternal sev baseApp req respRecieved =
  K.katipAddContext (K.sl "request" (toJSON . formatRequest $ req)) $ do
    K.logFM sev "Request received"
    baseApp req $ \resp -> do
      K.katipAddContext (K.sl "response" (toJSON . formatResponse $ resp)) $ do
        K.logFM sev "Reponse sent"
        respRecieved resp

-- runApplicationK :: (MonadIO m) => AppConfig -> ApplicationK m -> W.Application
-- runApplicationK appConf appK req respRecieved =  flip runReaderT appConf . unAppT $ (appK req (liftIO . respRecieved))

-- katipMiddleware :: LogConfig -> Severity -> W.Middleware
-- katipMiddleware logConf sev baseApp = runApplicationK logConf (katipMiddlewareInternal sev baseApp)