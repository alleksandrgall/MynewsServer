{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App where

import Auth (checkBasicAuth)
import Control.Concurrent (killThread)
import Control.Exception (Exception (fromException), bracket, throwIO)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Reader
import DB.Scheme (User)
import Data.Configurator as C
import Data.Configurator.Types (Config, onError)
import Data.Data (Proxy (Proxy))
import Dev (runDBDev)
import Katip
import Servant (BasicAuth, Handler, ServerError)
import Servant.Server
import System.Environment (getArgs)

newtype FatalConfigError = FatalConfigError String deriving (Show)

instance Exception FatalConfigError

withConfig :: (Config -> IO ()) -> IO ()
withConfig f =
  bracket
    ( do
        args <- getArgs
        case args of
          (confPath : _) -> autoReload (autoConfig {onError = throwIO}) [Required confPath]
          [] -> autoReload (autoConfig {onError = throwIO}) [Required "/home/turban/metaLampServer/config/configDev.cfg"]
    )
    (\(_, i) -> killThread i)
    (f . fst)

getConStr :: (MonadReader Config m, MonadIO m) => m String
getConStr = do
  conf <- ask
  maybeCon <- liftIO . C.lookup conf $ "connectionString"
  maybe (liftIO . throwIO $ FatalConfigError "DB connection string is required") return maybeCon

askImageRoot :: (MonadReader Config m, MonadIO m) => m String
askImageRoot = do
  conf <- ask
  maybeVal <- liftIO . C.lookup conf $ "imageRoot"
  maybe (liftIO . throwIO $ FatalConfigError "Location for image storage is required") return maybeVal

askImageSize :: (MonadReader Config m, MonadIO m) => m Integer
askImageSize = do
  conf <- ask
  liftIO . C.lookupDefault 20971520 conf $ "maxImageSize"

askPaginationLimit :: (MonadReader Config m, MonadIO m) => m Int
askPaginationLimit = do
  conf <- ask
  liftIO . C.lookupDefault 20 conf $ "paginationLimit"

askMaxImagesUpload :: (MonadReader Config m, MonadIO m) => m Int
askMaxImagesUpload = do
  conf <- ask
  liftIO . C.lookupDefault 30 conf $ "askMaxImagesUpload"

newtype AppT m a = AppT {unApp :: ReaderT Config (ExceptT ServerError m) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Config,
      MonadError ServerError,
      MonadIO
    )

type App = AppT IO

convertApp :: Config -> App a -> Handler a
convertApp c (AppT a) = Handler $ runReaderT a c

toServer :: (HasServer api '[BasicAuthCheck User]) => Proxy api -> Config -> ServerT api App -> Server api
toServer api conf = hoistServerWithContext api (Proxy :: Proxy '[BasicAuthCheck User]) (convertApp conf)

toApp :: HasServer api '[BasicAuthCheck User] => Proxy api -> ServerT api App -> Config -> Application
toApp api appServer conf = serveWithContext api ctx (toServer api conf appServer)
  where
    ctx = checkBasicAuth runDBDev :. EmptyContext