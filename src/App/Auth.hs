{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module App.Auth where

import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Crypto.KDF.BCrypt (validatePassword)
import DB.Scheme (Unique (UniqueUserName), User (userPasswordHash))
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
import Network.HTTP.Types (Header)
import qualified Network.Wai as W
import Servant
  ( AuthProtect,
    Context (EmptyContext, (:.)),
    Handler,
    ServerError (errHeaders, errReasonPhrase),
    err401,
    err404,
    throwError,
  )
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
  (forall a. Handler a) ->
  Handler (Entity User)
authenticate (username, pass) runDB onFail = do
  u <- liftIO $ runDB $ getBy . UniqueUserName . unpack . decodeUtf8 $ username
  case u of
    Nothing -> onFail
    Just ent -> do
      let validationResult = validatePassword pass (userPasswordHash . entityVal $ ent)
      if validationResult then return ent else onFail

adminAuthHandler :: (forall a. SqlPersistM a -> IO a) -> AuthHandler W.Request (Auth 'Admin)
adminAuthHandler runDB = mkAuthHandler $ \req -> do
  credentials <- maybe (throwError err404) return $ getCredentials req
  Auth <$> authenticate credentials runDB (throwError err404)

plzAuthHeader :: Header
plzAuthHeader = ("WWW-Authenticate", "Basic realm=\"User Visible Realm\"")

normalAuthHandler :: (forall a. SqlPersistM a -> IO a) -> AuthHandler W.Request (Auth 'Normal)
normalAuthHandler runDB = mkAuthHandler $ \req -> do
  credentials <-
    maybe
      (throwError err401 {errReasonPhrase = "Authentication required", errHeaders = [plzAuthHeader]})
      return
      $ getCredentials req
  Auth <$> authenticate credentials runDB (throwError err401 {errReasonPhrase = "Unauthorized"})

type instance AuthServerData (AuthProtect "admin") = Auth 'Admin

type instance AuthServerData (AuthProtect "normal") = Auth 'Normal

type AuthContext = '[AuthHandler W.Request (Auth 'Admin), AuthHandler W.Request (Auth 'Normal)]

authContext :: (forall a. SqlPersistM a -> IO a) -> Context AuthContext
authContext runDB = adminAuthHandler runDB :. normalAuthHandler runDB :. EmptyContext
