{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Dev where

import Control.Monad (unless, when)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Reader (MonadIO (liftIO))
import Crypto.KDF.BCrypt (hashPassword)
import DB.Scheme
import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), object)
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (getCurrentTime)
import Database.Persist.Postgresql (SqlPersistT, withPostgresqlPool)
import Database.Persist.Sql (SqlPersistM, liftSqlPersistMPool, runSqlPersistMPool, withSqlPool)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant

conStringDev = "host=localhost port=5432 user=turban dbname=news_db password=1781"

limitDev :: Int
limitDev = 20

maxFilesDev :: Int
maxFilesDev = 30

imageRootDev = "/home/turban/metaLampServer/images"

imageSizeDev :: Int64
imageSizeDev = 20971520

runDBDev ::
  (MonadIO m) => SqlPersistM a -> m a
runDBDev x = liftIO . runStderrLoggingT $
  withPostgresqlPool conStringDev 1 $ \pool -> liftSqlPersistMPool x pool

createUser ::
  String -> -- login
  String -> --pswd
  Maybe ImageId ->
  Bool -> --is admin
  Bool -> --is Author
  IO User
createUser log pswd av isAdm isAuth = do
  pswdH <- (hashPassword 6 $ encodeUtf8 . T.pack $ pswd :: IO BS.ByteString)
  date <- getCurrentTime
  return
    User
      { userName = log,
        userAvatar = av,
        userPasswordHash = pswdH,
        userCreated = date,
        userIsAdmin = isAdm,
        userIsAuthor = isAuth
      }