{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module App.App where

import App.Auth (AuthContext, authContext)
import Control.Concurrent (killThread)
import Control.Exception (throwIO)
import Control.Monad.Catch (Exception, MonadCatch, MonadThrow, bracket)
import Control.Monad.Except (MonadError (..), MonadIO (..))
import Control.Monad.Logger (NoLoggingT (runNoLoggingT))
import Control.Monad.Reader (MonadReader (local), ReaderT (..), asks)
import Data.Char (toLower)
import qualified Data.Configurator as C
import Data.Configurator.Types (AutoConfig (onError), Config, Configured (..), Value (String))
import Data.Int (Int64)
import Data.String (IsString (fromString))
import qualified Data.Text as T
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text.Lazy.IO as T
import Database.Persist.Postgresql (SqlPersistM, liftSqlPersistMPool, withPostgresqlPool)
import qualified Katip as K
import Servant
  ( Application,
    Handler,
    HasServer (ServerT, hoistServerWithContext),
    Proxy (..),
    Server,
    ServerError,
    serveWithContext,
  )
import System.Environment (getArgs)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)

newtype AppT m a = AppT {unApp :: ReaderT AppConfig m a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadThrow,
      MonadCatch,
      MonadReader AppConfig,
      MonadIO
    )

instance (MonadError ServerError m) => MonadError ServerError (AppT m) where
  throwError e = AppT . ReaderT $ \_ -> throwError e
  catchError m h = AppT . ReaderT $ \r -> catchError (flip runReaderT r . unApp $ m) (flip runReaderT r . unApp . h)

type App = AppT Handler

data AppConfig = AppConfig
  { generalConfig :: Config,
    logConfig :: LogConfig
  }

data LogConfig = LogConfig
  { logEnv :: K.LogEnv,
    logContext :: K.LogContexts,
    logNamespace :: K.Namespace
  }

newtype FatalConfigError = FatalConfigError String deriving (Show)

instance Exception FatalConfigError

mkAppConfig :: Config -> IO AppConfig
mkAppConfig c = do
  scrb <- mkScribeFromConfig c
  appScribeLogEnv <- K.registerScribe "live scribe" scrb K.defaultScribeSettings =<< K.initLogEnv "metaLampServer" "main"
  return $ AppConfig c (LogConfig appScribeLogEnv mempty mempty)

withAppConfig :: (AppConfig -> IO ()) -> IO ()
withAppConfig f =
  bracket
    ( do
        args <- getArgs
        (conf, threadId) <- case args of
          (confPath : _) -> C.autoReload (C.autoConfig {onError = throwIO}) [C.Required confPath]
          [] -> C.autoReload (C.autoConfig {onError = throwIO}) [C.Required "/home/turban/metaLampServer/config/configDev.cfg"]
        appConf <- mkAppConfig conf
        return (appConf, threadId)
    )
    (\(appConf, i) -> K.closeScribes (logEnv . logConfig $ appConf) >> killThread i)
    (f . fst)

askConStr :: (MonadReader AppConfig m, MonadIO m) => m String
askConStr = do
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

runDB :: (MonadIO m) => SqlPersistM a -> AppT m a
runDB x = askConStr >>= \s -> actuallyRunDb s x

actuallyRunDb :: (MonadIO m) => String -> SqlPersistM a -> m a
actuallyRunDb conStr x = liftIO . runNoLoggingT $
  withPostgresqlPool (fromString conStr) 1 $ \pool -> liftSqlPersistMPool x pool

--Newtypes for working around of orphan instance warning
newtype MySeverity = MySeverity K.Severity

instance Configured MySeverity where
  convert (String s) =
    case T.map toLower s of
      "debug" -> Just $ MySeverity K.DebugS
      "info" -> Just $ MySeverity K.InfoS
      "warn" -> Just $ MySeverity K.WarningS
      "error" -> Just $ MySeverity K.ErrorS
      "notice" -> Just $ MySeverity K.NoticeS
      _ -> Nothing
  convert _ = Nothing

newtype MyVerbosity = MyVerbosity K.Verbosity

instance Configured MyVerbosity where
  convert (String s) =
    case T.map toLower s of
      "v0" -> Just $ MyVerbosity K.V0
      "v1" -> Just $ MyVerbosity K.V1
      "v2" -> Just $ MyVerbosity K.V2
      "v3" -> Just $ MyVerbosity K.V3
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

mkScribeFromConfig :: Config -> IO K.Scribe
mkScribeFromConfig cnf = return $ K.Scribe write finale permit
  where
    write :: forall a. K.LogItem a => K.Item a -> IO ()
    write i = do
      out <- C.lookupDefault Stdout cnf "logOut"
      (MyVerbosity verb) <- C.lookupDefault (MyVerbosity K.V2) cnf "logVerb"
      case out of
        Silent -> return ()
        Stdout -> do
          hSetBuffering stdout LineBuffering
          T.hPutStrLn stdout $ toLazyText $ K.bracketFormat True verb i
        Stderr -> do
          hSetBuffering stderr LineBuffering
          T.hPutStrLn stderr $ toLazyText $ K.bracketFormat True verb i
    finale :: IO ()
    finale = return ()
    permit :: K.PermitFunc
    permit i = do
      (MySeverity sev) <- C.lookupDefault (MySeverity K.InfoS) cnf "logLevel"
      K.permitItem sev i

instance (MonadIO m) => K.Katip (AppT m) where
  getLogEnv = asks (logEnv . logConfig)
  localLogEnv f (AppT m) = AppT $ local (\(AppConfig conf (LogConfig env cont names)) -> AppConfig conf (LogConfig (f env) cont names)) m

instance (MonadIO m) => K.KatipContext (AppT m) where
  getKatipContext = asks (logContext . logConfig)
  localKatipContext f (AppT m) = AppT (local (\s -> s {logConfig = (logConfig s) {logContext = f . logContext . logConfig $ s}}) m)
  getKatipNamespace = asks (logNamespace . logConfig)
  localKatipNamespace f (AppT m) = AppT (local (\s -> s {logConfig = (logConfig s) {logNamespace = f . logNamespace . logConfig $ s}}) m)

convertApp :: AppConfig -> App a -> Handler a
convertApp c (AppT a) = runReaderT a c

toServer :: HasServer api AuthContext => Proxy api -> AppConfig -> ServerT api App -> Server api
toServer api conf = hoistServerWithContext api (Proxy @AuthContext) (convertApp conf)

serveApp :: HasServer api AuthContext => Proxy api -> ServerT api App -> AppConfig -> Application
serveApp api appServer conf = serveWithContext api (authContext dbRunner) (toServer api conf appServer)
  where
    dbRunner :: forall a. SqlPersistM a -> IO a
    dbRunner = \query -> flip runReaderT conf $ do
      conStr <- askConStr
      liftIO $ actuallyRunDb conStr query
