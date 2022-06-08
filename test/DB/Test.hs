{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module DB.Test where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT (runNoLoggingT))
import Database.Persist.Sql (SqlPersistM)
import Database.Persist.Sqlite (runMigration, runSqlConn, withSqliteConn)
import Handlers.DB (Handler (..))
import Handlers.DB.Scheme (migrateAll)

-- makeSqlLiteMemCon :: (BackendCompatible SqlBackend backend) => (backend -> IO a) -> IO a
-- makeSqlLiteMemCon f = runNoLoggingT $ withSqlConn ":memory:" $ \conn -> f conn

-- withHandler :: (Handler -> IO a) -> IO a
-- withHandler f = do
--   runNoLoggingT $
--     withSqliteConn ":memory:" $ \conn -> do
--       liftIO $ runSqlConn (runMigration migrateAll) conn
--       let h = Handler
--             { hRunDB = \x -> liftIO $ runSqlConn x conn
--             }
--       liftIO $
--         f $ undefined