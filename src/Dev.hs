{-# LANGUAGE RankNTypes #-}

module Dev where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT (runNoLoggingT), runStdoutLoggingT)
import Database.Esqueleto.Experimental (SqlPersistM, liftSqlPersistMPool)
import Database.Persist.Postgresql (withPostgresqlPool)

runDBDev :: forall a. SqlPersistM a -> IO a
runDBDev x = liftIO . runStdoutLoggingT $
  withPostgresqlPool "host=localhost port=5432 user=turban dbname=news_db password=1781" 1 $ \pool -> liftSqlPersistMPool x pool