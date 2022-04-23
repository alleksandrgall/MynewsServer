{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Dev where

import Auth
import Control.Monad (unless, when)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Reader (MonadIO (liftIO))
import Crypto.KDF.BCrypt (hashPassword)
import DB.Scheme
import Data.Aeson (ToJSON)
import qualified Data.ByteString as BS
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (getCurrentTime)
import Database.Persist.Postgresql (withPostgresqlPool)
import Database.Persist.Sql (SqlPersistM, runSqlPersistMPool)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant

connectionString = "host=localhost port=5432 user=turban dbname=news_db password=1781"

maxFiles :: Int
maxFiles = 30

imageRoot = "/home/turban/metaLampServer/images"

maxImageSize :: Int64
maxImageSize = 20971520

data Ok = Ok
  { ok :: Bool,
    description :: String
  }
  deriving (Show, Generic)

instance ToJSON Ok

runDev :: forall api. HasServer api '[BasicAuthCheck User] => Proxy api -> Server api -> IO ()
runDev api server = run 3000 (serveWithContext api ctx server)
  where
    ctx = checkBasicAuth runDBDev :. EmptyContext

runDBDev ::
  SqlPersistM a ->
  IO a
runDBDev x = runStderrLoggingT $
  withPostgresqlPool connectionString 1 $ \pool -> liftIO $ do
    runSqlPersistMPool x pool

userIsAdmin_ :: User -> Handler ()
userIsAdmin_ User {..} = unless userIsAdmin $ throwError err404

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