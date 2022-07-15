module Handlers.DB where

import Control.Monad (void)
import Crypto.KDF.BCrypt (hashPassword)
import qualified Data.ByteString as BS
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime (utctDay), getCurrentTime)
import qualified Database.Persist as P
import Database.Persist.Sql (SqlPersistM)
import Handlers.DB.Scheme (User (..))

data Handler = Handler
  { hRunDB :: forall a. SqlPersistM a -> IO a,
    hMigrate :: IO ()
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

createDefaultAdmin :: Handler -> IO ()
createDefaultAdmin h = makeAdmin >>= void . hRunDB h . P.insertUnique