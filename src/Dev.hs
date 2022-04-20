module Dev where

import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Reader (MonadIO (liftIO))
import Database.Persist.Postgresql (withPostgresqlPool)
import Database.Persist.Sql (SqlPersistM, runSqlPersistMPool)

connectionString = "host=localhost port=5432 user=turban dbname=news_db password=1781"

runDBDev ::
  SqlPersistM a ->
  IO a
runDBDev x = runStderrLoggingT $
  withPostgresqlPool connectionString 1 $ \pool -> liftIO $ do
    runSqlPersistMPool x pool
