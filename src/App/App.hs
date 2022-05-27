{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module App.App where

import App.Auth (checkBasicAuth)
import Control.Concurrent (killThread)
import Control.Exception (throwIO)
import Control.Monad.Catch (Exception, MonadCatch, MonadThrow, bracket)
import Control.Monad.Except (MonadError (..), MonadIO (..))
import Control.Monad.Logger (NoLoggingT (runNoLoggingT))
import Control.Monad.Reader (MonadReader (local), ReaderT (..), asks)
import DB.Scheme (User (User))
import Data.Char (toLower)
import qualified Data.Configurator as C
import Data.Configurator.Types (AutoConfig (onError), Config, Configured (..), Value (String))
import Data.Int (Int64)
import Data.String (IsString (fromString))
import qualified Data.Text as T
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text.Lazy.IO as T
import Database.Persist.Postgresql (Entity, SqlPersistM, liftSqlPersistMPool, withPostgresqlPool)
import qualified Katip as K
import Servant
  ( Application,
    BasicAuthCheck,
    Context (EmptyContext, (:.)),
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
  throwError e = AppT . ReaderT $ \r -> throwError e
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
  logEnv <- K.registerScribe "live scribe" scrb K.defaultScribeSettings =<< K.initLogEnv "metaLampServer" "main"
  return $ AppConfig c (LogConfig logEnv mempty mempty)

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
    ( \(appConf, i) -> do
        K.closeScribes (logEnv . logConfig $ appConf)
        killThread i
    )
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

instance Configured K.Severity where
  convert (String s) =
    case T.map toLower s of
      "debug" -> Just K.DebugS
      "info" -> Just K.InfoS
      "warn" -> Just K.WarningS
      "error" -> Just K.ErrorS
      "notice" -> Just K.NoticeS
      _ -> Nothing
  convert _ = Nothing

instance Configured K.Verbosity where
  convert (String s) =
    case T.map toLower s of
      "v0" -> Just K.V0
      "v1" -> Just K.V1
      "v2" -> Just K.V2
      "v3" -> Just K.V3
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
      verb <- C.lookupDefault K.V2 cnf "logVerb"
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
      sev <- C.lookupDefault K.InfoS cnf "logLevel"
      K.permitItem sev i

instance (MonadIO m) => K.Katip (AppT m) where
  getLogEnv = asks (logEnv . logConfig)
  localLogEnv f (AppT m) = AppT $ local (\(AppConfig conf (LogConfig env cont names)) -> AppConfig conf (LogConfig (f env) cont names)) m

instance (MonadIO m) => K.KatipContext (AppT m) where
  getKatipContext = asks (logContext . logConfig)
  localKatipContext f (AppT m) = AppT (local (\s -> s {logConfig = (logConfig s) {logContext = f . logContext . logConfig $ s}}) m)
  getKatipNamespace = asks (logNamespace . logConfig)
  localKatipNamespace f (AppT m) = AppT (local (\s -> s {logConfig = (logConfig s) {logNamespace = f . logNamespace . logConfig $ s}}) m)

type AuthContext = BasicAuthCheck (Entity User)

convertApp :: AppConfig -> App a -> Handler a
convertApp c (AppT a) = runReaderT a c

toServer :: HasServer api '[AuthContext] => Proxy api -> AppConfig -> ServerT api App -> Server api
toServer api conf = hoistServerWithContext api (Proxy @'[AuthContext]) (convertApp conf)

serveApp :: HasServer api '[AuthContext] => Proxy api -> ServerT api App -> AppConfig -> Application
serveApp api appServer conf = serveWithContext api ctx (toServer api conf appServer)
  where
    ctx = checkBasicAuth dbRunner :. EmptyContext
    dbRunner :: forall a. SqlPersistM a -> IO a
    dbRunner = \query -> flip runReaderT conf $ do
      conStr <- askConStr
      liftIO $ actuallyRunDb conStr query
