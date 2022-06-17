{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Handlers.App.App where

import Control.Monad (join)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadIO, MonadReader (local), ReaderT (ReaderT), asks)
import Database.Persist.Sql (SqlPersistM)
import GHC.Natural (Natural)
import qualified Handlers.DB as DB
import qualified Handlers.Image as I
import qualified Handlers.Katip as L
import qualified Katip as K
import qualified Servant as S

newtype App imageM a = App {unApp :: ReaderT (Handler imageM) (ExceptT S.ServerError IO) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadThrow,
      MonadCatch,
      MonadReader (Handler imageM),
      MonadError S.ServerError,
      MonadIO
    )

data Config imageM = Config
  {cPaginationLimit :: App imageM Natural}

data Handler m = Handler
  { hConfig :: Config m,
    hDBHandler :: DB.Handler,
    hKatipHandler :: L.Handler,
    hImageHandler :: I.Handler m
  }

askPaginationLimit :: App imageM Natural
askPaginationLimit = join (asks (cPaginationLimit . hConfig))

runDB :: SqlPersistM a -> App imageM a
runDB x = asks (DB.hRunDB . hDBHandler) >>= liftIO . flip ($) x

instance K.Katip (App imageM) where
  getLogEnv = asks (L.cLogEnv . L.hConfig . hKatipHandler)
  localLogEnv f (App m) =
    App $
      flip local m $
        \s -> s {hKatipHandler = (hKatipHandler s) {L.hConfig = (L.hConfig . hKatipHandler $ s) {L.cLogEnv = f (L.cLogEnv . L.hConfig . hKatipHandler $ s)}}}

instance K.KatipContext (App imageM) where
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
