{-# LANGUAGE ScopedTypeVariables #-}

module Handlers.App.Auth
  ( authContext,
    Auth (..),
    AuthContext,
  )
where

import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Crypto.KDF.BCrypt (validatePassword)
import qualified Data.ByteString as BS
import Data.ByteString.Base64 (decodeLenient)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Word8 (isSpace, toLower, _colon)
import Database.Persist.Sql
  ( Entity (entityVal),
    PersistUniqueRead (getBy),
    SqlPersistM,
  )
import Handlers.App.App (Handler (..))
import qualified Handlers.DB as DB
import Handlers.DB.Scheme (Unique (UniqueUserName), User (userPasswordHash))
import Network.HTTP.Types (Header)
import qualified Network.Wai as W
import qualified Servant as S
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)

data Level = Normal | Admin

newtype Auth (a :: Level) = Auth (Entity User)

getCredentials :: W.Request -> Maybe (BS.ByteString, BS.ByteString)
getCredentials req = do
  authHeader <- lookup "Authorization" $ W.requestHeaders req
  let (b, rest) = BS.break isSpace authHeader
  guard (BS.map toLower b == "basic")
  let decoded = decodeLenient (BS.dropWhile isSpace rest)
      (username, passWithColonAhead) = BS.break (== _colon) decoded
      pass = BS.dropWhile (== _colon) passWithColonAhead
  return (username, pass)

authenticate ::
  (BS.ByteString, BS.ByteString) ->
  (forall a. SqlPersistM a -> IO a) ->
  (forall a. S.Handler a) ->
  S.Handler (Entity User)
authenticate (username, pass) runDB onFail = do
  u <- liftIO $ runDB $ getBy . UniqueUserName . unpack . decodeUtf8 $ username
  case u of
    Nothing -> onFail
    Just ent -> do
      let validationResult = validatePassword pass (userPasswordHash . entityVal $ ent)
      if validationResult then return ent else onFail

adminAuthHandler :: Handler imageM -> AuthHandler W.Request (Auth 'Admin)
adminAuthHandler Handler {..} = mkAuthHandler $ \req -> do
  credentials <- maybe (S.throwError S.err404) return $ getCredentials req
  Auth <$> authenticate credentials (DB.hRunDB hDBHandler) (S.throwError S.err404)

plzAuthHeader :: Header
plzAuthHeader = ("WWW-Authenticate", "Basic realm=\"User Visible Realm\"")

normalAuthHandler :: Handler imageM -> AuthHandler W.Request (Auth 'Normal)
normalAuthHandler Handler {..} = mkAuthHandler $ \req -> do
  credentials <- maybe (S.throwError S.err401 {S.errReasonPhrase = "Authentication required.", S.errHeaders = [plzAuthHeader]}) return $ getCredentials req
  Auth <$> authenticate credentials (DB.hRunDB hDBHandler) (S.throwError S.err401 {S.errReasonPhrase = "Unauthorized"})

{- Issue https://github.com/haskell/haskell-language-server/issues/773 is still unfixed-}
type instance AuthServerData (S.AuthProtect "admin") = Auth 'Admin

type instance AuthServerData (S.AuthProtect "normal") = Auth 'Normal

type AuthContext = '[AuthHandler W.Request (Auth 'Admin), AuthHandler W.Request (Auth 'Normal)]

authContext :: Handler imageM -> S.Context AuthContext
authContext h = adminAuthHandler h S.:. normalAuthHandler h S.:. S.EmptyContext
