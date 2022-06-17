{-# LANGUAGE RankNTypes #-}

module Handlers.DB where

import Control.Monad (void)
import Crypto.KDF.BCrypt (hashPassword)
import qualified Data.ByteString as BS
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime (utctDay), getCurrentTime)
import qualified Database.Persist as P
import Database.Persist.Sql (SqlPersistM)
import qualified Database.Persist.Sql as P
import Handlers.DB.Scheme (User (..), migrateAll)

newtype Handler = Handler
  { hRunDB :: forall a. SqlPersistM a -> IO a
  }

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

migrate_ :: Handler -> IO ()
migrate_ h = do
  admin <- makeAdmin
  void $
    hRunDB h $ do
      void $ P.insert admin
      P.runMigration migrateAll