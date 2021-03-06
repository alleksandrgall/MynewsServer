{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Handlers.Katip.Middleware where

import Control.Monad.Reader (MonadIO (..), MonadReader (ask, local), ReaderT (..), asks)
import Data.Aeson ((.=))
import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import Handlers.Katip (Config (cLogContexts, cLogEnv, cLogNamespace), Handler (hConfig))
import qualified Katip as K
import Network.HTTP.Types (HttpVersion, Method, Query, Status, queryToQueryText)
import qualified Network.Wai as W

newtype KatipM a = KatipM {unKatipM :: ReaderT Handler IO a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Handler,
      MonadIO
    )

instance K.Katip KatipM where
  getLogEnv = asks (cLogEnv . hConfig)
  localLogEnv f (KatipM m) = KatipM $ local (\s -> s {hConfig = (hConfig s) {cLogEnv = f . cLogEnv . hConfig $ s}}) m

instance K.KatipContext KatipM where
  getKatipContext = asks (cLogContexts . hConfig)
  localKatipContext f (KatipM m) = KatipM $ local (\s -> s {hConfig = (hConfig s) {cLogContexts = f . cLogContexts . hConfig $ s}}) m
  getKatipNamespace = asks (cLogNamespace . hConfig)
  localKatipNamespace f (KatipM m) = KatipM $ local (\s -> s {hConfig = (hConfig s) {cLogNamespace = f . cLogNamespace . hConfig $ s}}) m

type ApplicationK = W.Request -> (W.Response -> KatipM W.ResponseReceived) -> KatipM W.ResponseReceived

type MiddlewareK = ApplicationK -> ApplicationK

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

mkApplicationK :: (Handler -> W.Application) -> ApplicationK
mkApplicationK f req respReceived = do
  localLogConfig <- ask
  liftIO $ f localLogConfig req (flip runReaderT localLogConfig . unKatipM . respReceived)

runApplicationK :: Handler -> ApplicationK -> W.Application
runApplicationK logConf app req respReceived = flip runReaderT logConf . unKatipM $ app req (liftIO . respReceived)