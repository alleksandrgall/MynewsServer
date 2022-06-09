{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Handlers.App.App where

import Control.Monad (join)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadIO, MonadReader (local), ReaderT (ReaderT), asks)
import Data.Int (Int64)
import Database.Persist.Sql (SqlPersistM)
import GHC.Natural (Natural)
import qualified Handlers.DB as DB
import qualified Handlers.Katip as L
import qualified Katip as K
import qualified Servant as S

newtype App a = App {unApp :: ReaderT Handler (ExceptT S.ServerError IO) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadThrow,
      MonadCatch,
      MonadReader Handler,
      MonadError S.ServerError,
      MonadIO
    )

data Config = Config
  { cImageRoot :: App FilePath,
    cMaxImageSize :: App Int64,
    cPaginationLimit :: App Natural,
    cMaxImagesUpload :: App Int
  }

data Handler = Handler
  { hConfig :: Config,
    hDBHandler :: DB.Handler,
    hKatipHandler :: L.Handler
  }

askImageRoot :: App FilePath
askImageRoot = join $ asks (cImageRoot . hConfig)

askMaxImageSize :: App Int64
askMaxImageSize = join (asks (cMaxImageSize . hConfig))

askPaginationLimit :: App Natural
askPaginationLimit = join (asks (cPaginationLimit . hConfig))

askMaxImagesUpload :: App Int
askMaxImagesUpload = join (asks (cMaxImagesUpload . hConfig))

runDB :: SqlPersistM a -> App a
runDB x = asks (DB.hRunDB . hDBHandler) >>= liftIO . flip ($) x

instance K.Katip App where
  getLogEnv = asks (L.cLogEnv . L.hConfig . hKatipHandler)
  localLogEnv f (App m) =
    App $
      flip local m $
        \s -> s {hKatipHandler = (hKatipHandler s) {L.hConfig = (L.hConfig . hKatipHandler $ s) {L.cLogEnv = f (L.cLogEnv . L.hConfig . hKatipHandler $ s)}}}

instance K.KatipContext App where
  getKatipContext = asks (L.cLogContexts . L.hConfig . hKatipHandler)
  localKatipContext f (App m) =
    App $
      flip local m $
        \s -> s {hKatipHandler = (hKatipHandler s) {L.hConfig = (L.hConfig . hKatipHandler $ s) {L.cLogContexts = f (L.cLogContexts . L.hConfig . hKatipHandler $ s)}}}
  getKatipNamespace = asks (L.cLogNamespace . L.hConfig . hKatipHandler)
  localKatipNamespace f (App m) =
    App $
      flip local m $
        \s -> s {hKatipHandler = (hKatipHandler s) {L.hConfig = (L.hConfig . hKatipHandler $ s) {L.cLogNamespace = f (L.cLogNamespace . L.hConfig . hKatipHandler $ s)}}}
