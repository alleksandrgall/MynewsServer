{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module DB.Test (Handler, withHandler) where

import Conduit (runResourceT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT (runNoLoggingT))
import Crypto.KDF.BCrypt (hashPassword)
import qualified Data.ByteString as BS
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime (utctDay), getCurrentTime)
import qualified Database.Persist as P
import Database.Persist.Sql (SqlPersistM)
import Database.Persist.Sqlite (runMigration, runSqlConn, withSqliteConn)
import Handlers.DB (Handler (..))
import Handlers.DB.Scheme (User (..), migrateAll)

makeAdmin :: IO User
makeAdmin = do
  pswdH <- (hashPassword 6 $ encodeUtf8 "admin" :: IO BS.ByteString)
  date <- utctDay <$> getCurrentTime
  return
    User
      { userName = "admin",
        userAvatar = Nothing,
        userPasswordHash = pswdH,
        userCreated = date,
        userIsAdmin = True,
        userIsAuthor = True
      }

--Creates DB in memory, makes initial migrations and adds an admin for testing admin api
withHandler :: (Handler -> IO a) -> IO a
withHandler f = do
  runNoLoggingT $
    withSqliteConn ":memory:" $ \conn -> do
      liftIO $ runSqlConn (runMigration migrateAll) conn
      admin <- liftIO makeAdmin
      liftIO $ runSqlConn (P.insert admin) conn
      let h =
            Handler
              { hRunDB = \x -> runResourceT . runNoLoggingT $ runSqlConn x conn
              }
      liftIO $
        f h