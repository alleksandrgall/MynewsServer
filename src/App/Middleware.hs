{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module App.Middleware where

import App.App
import Auth
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask, local), ReaderT (runReaderT), asks)
import DB.Scheme
import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), Value, object)
import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Database.Esqueleto (Entity, SqlPersistM)
import GHC.Generics (Generic)
import Katip (Katip (getLogEnv), KatipContext (localKatipContext), Severity, katipAddContext, logFM, sl)
import Katip.Core (Katip (localLogEnv))
import Katip.Monadic (KatipContext (getKatipContext, getKatipNamespace, localKatipNamespace))
import Network.HTTP.Types (HttpVersion, Method, Query, Status, queryToQueryText)
import qualified Network.Wai as W
import Servant
import UnliftIO (MonadUnliftIO (withRunInIO))

newtype KatipM a = KatipM {unKatipM :: ReaderT LogConfig IO a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader LogConfig,
      MonadIO,
      MonadUnliftIO
    )

instance Katip KatipM where
  getLogEnv = asks logEnv
  localLogEnv f (KatipM m) = KatipM $ local (\s -> s {logEnv = f $ logEnv s}) m

instance KatipContext KatipM where
  getKatipContext = asks logContext
  localKatipContext f (KatipM m) = KatipM (local (\s -> s {logContext = f $ logContext s}) m)
  getKatipNamespace = asks logNamespace
  localKatipNamespace f (KatipM m) = KatipM (local (\s -> s {logNamespace = f $ logNamespace s}) m)

type ApplicationM = W.Request -> (W.Response -> KatipM W.ResponseReceived) -> KatipM W.ResponseReceived

type MiddlewareM = ApplicationM -> ApplicationM

toApplicationIO :: W.Application -> ApplicationM
toApplicationIO app req respReceived = withRunInIO $ \inner -> app req (inner . respReceived)

runApplicationM :: LogConfig -> ApplicationM -> W.Application
runApplicationM appConf app req respReceived = flip runReaderT appConf . unKatipM $ app req (liftIO . respReceived)

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

katipMiddlewareInternal :: Severity -> MiddlewareM
katipMiddlewareInternal sev baseApp req respRecieved =
  katipAddContext (sl "request" (toJSON . formatRequest $ req)) $ do
    logFM sev "Request received"
    baseApp req $ \resp ->
      katipAddContext (sl "response" (toJSON . formatResponse $ resp)) $ do
        logFM sev "Reponse sent"
        respRecieved resp

katipMiddleware :: LogConfig -> Severity -> W.Middleware
katipMiddleware appConf sev baseApp = runApplicationM appConf (katipMiddlewareInternal sev (toApplicationIO baseApp))

mkApplication ::
  ( HasServer api context,
    ( HasContextEntry
        (context .++ DefaultErrorFormatters)
        ErrorFormatters
    )
  ) =>
  (AppConfig -> Context context) ->
  Proxy context ->
  Proxy api ->
  ServerT api App ->
  AppConfig ->
  ApplicationM
mkApplication getCtxt prCtxt prApi serverApp appConf = \req respReceived -> do
  logConf <- ask
  let hoistedApp =
        let toHandler = convertApp appConf {logConfig = logConf} --- !!!!
            hoistedServer = hoistServerWithContext prApi prCtxt toHandler serverApp
         in serveWithContext prApi (getCtxt appConf) hoistedServer
  withRunInIO $ \toIO -> hoistedApp req (toIO . respReceived)

authContext :: AppConfig -> Context '[BasicAuthCheck (Entity User)]
authContext conf = checkBasicAuth dbRunner :. EmptyContext
  where
    dbRunner :: forall a. SqlPersistM a -> IO a
    dbRunner = \query -> flip runReaderT conf $ do
      conStr <- getConStr
      liftIO $ actuallyRunDb conStr query

prCtxt :: Proxy '[BasicAuthCheck (Entity User)]
prCtxt = Proxy

mkApplication' ::
  ( HasServer api '[BasicAuthCheck (Entity User)]
  ) =>
  Proxy api ->
  ServerT api App ->
  AppConfig ->
  ApplicationM
mkApplication' = mkApplication authContext prCtxt