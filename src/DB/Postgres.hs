module DB.Postgres (withHandler) where

import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT (runNoLoggingT))
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.String (IsString (fromString))
import Database.Persist.Postgresql (withPostgresqlPool)
import Database.Persist.Sql
import Handlers.DB

newtype FatalNoDBConnectionString = FatalNoDBConnectionString String deriving (Show)

instance Exception FatalNoDBConnectionString

askConStr :: C.Config -> IO String
askConStr c = do
  maybeCon <- liftIO . C.lookup c $ "connectionString"
  maybe (liftIO . throwIO $ FatalNoDBConnectionString "DB connection string is required") return maybeCon

runDBPostgres :: String -> SqlPersistM a -> IO a
runDBPostgres conStr x = liftIO . runNoLoggingT $
  withPostgresqlPool (fromString conStr) 1 $ \pool -> liftSqlPersistMPool x pool

withHandler :: C.Config -> (Handler -> IO a) -> IO a
withHandler conf f =
  f $
    Handler
      { hRunDB = \x -> do
          conStr <- askConStr conf
          runDBPostgres conStr x
      }
