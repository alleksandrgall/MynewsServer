{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App where

import Auth (checkBasicAuth)
import Control.Concurrent (killThread)
import Control.Exception (Exception (fromException), bracket, throwIO)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Logger
import Control.Monad.Reader
import DB.Scheme (User)
import Data.Char (toLower)
import Data.Configurator as C
import Data.Configurator.Types (Config, Configured (convert), Value (List, String), onError)
import Data.Data (Proxy (Proxy))
import Data.Int (Int64)
import Data.Monoid hiding (mempty)
import qualified Data.Text as T (map)
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text.Lazy.IO as T
import Database.Persist (Entity)
import Dev (runDBDev)
import Katip
import Network.Wai
import Servant (BasicAuth, Handler, ServerError)
import Servant.Server
import System.Environment (getArgs)
import System.IO (BufferMode (LineBuffering), IOMode (WriteMode), hSetBuffering, stderr, stdout)

newtype AppT m a = AppT {unApp :: ReaderT AppConfig (ExceptT ServerError m) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadThrow,
      MonadCatch,
      MonadReader AppConfig,
      MonadError ServerError,
      MonadIO
    )

type App = AppT IO

data AppConfig = AppConfig
  { generalConfig :: Config,
    logConfig :: LogConfig
  }

data LogConfig = LogConfig
  { logEnv :: LogEnv,
    logContext :: LogContexts,
    logNamespace :: Namespace
  }

newtype FatalConfigError = FatalConfigError String deriving (Show)

instance Exception FatalConfigError

mkAppConfig :: Config -> IO AppConfig
mkAppConfig c = do
  scrb <- mkScribeFromConfig c
  logEnv <- registerScribe "live scribe" scrb defaultScribeSettings =<< initLogEnv "metaLampServer" "main"
  return $ AppConfig c (LogConfig logEnv mempty mempty)

withAppConfig :: (AppConfig -> IO ()) -> IO ()
withAppConfig f =
  bracket
    ( do
        args <- getArgs
        (conf, threadId) <- case args of
          (confPath : _) -> autoReload (autoConfig {onError = throwIO}) [Required confPath]
          [] -> autoReload (autoConfig {onError = throwIO}) [Required "/home/turban/metaLampServer/config/configDev.cfg"]
        appConf <- mkAppConfig conf
        return (appConf, threadId)
    )
    ( \(appConf, i) -> do
        closeScribes (logEnv . logConfig $ appConf)
        killThread i
    )
    (f . fst)

getConStr :: (MonadReader AppConfig m, MonadIO m) => m String
getConStr = do
  conf <- asks generalConfig
  maybeCon <- liftIO . C.lookup conf $ "connectionString"
  maybe (liftIO . throwIO $ FatalConfigError "DB connection string is required") return maybeCon

askImageRoot :: (MonadReader AppConfig m, MonadIO m) => m String
askImageRoot = do
  conf <- asks generalConfig
  maybeVal <- liftIO . C.lookup conf $ "imageRoot"
  maybe (liftIO . throwIO $ FatalConfigError "Location for image storage is required") return maybeVal

askMaxImageSize :: (MonadReader AppConfig m, MonadIO m) => m Int64
askMaxImageSize = do
  conf <- asks generalConfig
  liftIO . C.lookupDefault 20971520 conf $ "maxImageSize"

askPaginationLimit :: (MonadReader AppConfig m, MonadIO m) => m Int
askPaginationLimit = do
  conf <- asks generalConfig
  liftIO . C.lookupDefault 20 conf $ "paginationLimit"

askMaxImagesUpload :: (MonadReader AppConfig m, MonadIO m) => m Int
askMaxImagesUpload = do
  conf <- asks generalConfig
  liftIO . C.lookupDefault 30 conf $ "maxImagesUpload"

logLevelToSeverity :: LogLevel -> Severity
logLevelToSeverity LevelDebug = DebugS
logLevelToSeverity LevelInfo = InfoS
logLevelToSeverity LevelWarn = WarningS
logLevelToSeverity LevelError = ErrorS
logLevelToSeverity (LevelOther _) = NoticeS

instance Configured LogLevel where
  convert (String s) =
    case T.map toLower s of
      "debug" -> Just LevelDebug
      "info" -> Just LevelInfo
      "warn" -> Just LevelWarn
      "error" -> Just LevelError
      "notice" -> Just $ LevelOther ""
      _ -> Nothing
  convert _ = Nothing

instance Configured Severity where
  convert (String s) =
    case T.map toLower s of
      "debug" -> Just DebugS
      "info" -> Just InfoS
      "warn" -> Just WarningS
      "error" -> Just ErrorS
      "notice" -> Just NoticeS
      _ -> Nothing
  convert _ = Nothing

instance Configured Verbosity where
  convert (String s) =
    case T.map toLower s of
      "v0" -> Just V0
      "v1" -> Just V1
      "v2" -> Just V2
      "v3" -> Just V3
      _ -> Nothing
  convert _ = Nothing

data LogOut = Stdout | Stderr | Silent

instance Configured LogOut where
  convert (String s) =
    case T.map toLower s of
      "stdout" -> Just Stdout
      "stderr" -> Just Stderr
      "silent" -> Just Silent
      _ -> Nothing
  convert _ = Nothing

mkScribeFromConfig :: Config -> IO Scribe
mkScribeFromConfig cnf = return $ Scribe write finale permit
  where
    write :: forall a. LogItem a => Item a -> IO ()
    write i = do
      out <- C.lookupDefault Stdout cnf "logLevel"
      verb <- C.lookupDefault V2 cnf "logVerb"
      case out of
        Silent -> return ()
        Stdout -> do
          hSetBuffering stdout LineBuffering
          T.hPutStrLn stdout $ toLazyText $ bracketFormat True verb i
        Stderr -> do
          hSetBuffering stderr LineBuffering
          T.hPutStrLn stderr $ toLazyText $ bracketFormat True verb i
    finale :: IO ()
    finale = return ()
    permit :: PermitFunc
    permit i = do
      sev <- C.lookupDefault InfoS cnf "logLevel"
      permitItem sev i

instance (MonadIO m) => Katip (AppT m) where
  getLogEnv = asks (logEnv . logConfig)
  localLogEnv f (AppT m) = AppT $ local (\(AppConfig conf (LogConfig env cont names)) -> AppConfig conf (LogConfig (f env) cont names)) m

instance (MonadIO m) => KatipContext (AppT m) where
  getKatipContext = asks (logContext . logConfig)
  localKatipContext f (AppT m) = AppT (local (\s -> s {logConfig = (logConfig s) {logContext = f . logContext . logConfig $ s}}) m)
  getKatipNamespace = asks (logNamespace . logConfig)
  localKatipNamespace f (AppT m) = AppT (local (\s -> s {logConfig = (logConfig s) {logNamespace = f . logNamespace . logConfig $ s}}) m)

-- Maybe later will implement with katip, write now seems sus
-- katipMiddleware :: AppConfig -> Middleware
-- katipMiddleware AppConfig {..} app = \req respond ->

convertApp :: AppConfig -> App a -> Handler a
convertApp c (AppT a) = Handler $ runReaderT a c

toServer :: HasServer api '[BasicAuthCheck (Entity User)] => Proxy api -> AppConfig -> ServerT api App -> Server api
toServer api conf = hoistServerWithContext api (Proxy :: Proxy '[BasicAuthCheck (Entity User)]) (convertApp conf)

toApp :: HasServer api '[BasicAuthCheck (Entity User)] => Proxy api -> ServerT api App -> AppConfig -> Application
toApp api appServer conf = serveWithContext api ctx (toServer api conf appServer)
  where
    ctx = checkBasicAuth runDBDev :. EmptyContext