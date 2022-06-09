{-# LANGUAGE RankNTypes #-}

module Handlers.DB where

import Database.Persist.Sql (SqlPersistM)

newtype Handler = Handler
  { hRunDB :: forall a. SqlPersistM a -> IO a
  }
