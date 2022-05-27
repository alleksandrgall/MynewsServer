{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module App.Auth where

import Control.Monad (unless)
import Crypto.KDF.BCrypt (validatePassword)
import DB.Scheme
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Database.Persist.Sql
import Servant

data Role = NormalUser | Author | Admin

data AuthUserAtLeast (a :: Role) = AuthUserAtLeast (Entity User)

checkBasicAuthAdmin :: (forall a. SqlPersistM a -> IO a) -> BasicAuthCheck (AuthUserAtLeast 'Admin)
checkBasicAuthAdmin runDB = undefined

checkBasicAuthAuthor :: (forall a. SqlPersistM a -> IO a) -> BasicAuthCheck (AuthUserAtLeast 'Author)
checkBasicAuthAuthor runDB = undefined

checkBasicAuthNormal :: (forall a. SqlPersistM a -> IO a) -> BasicAuthCheck (AuthUserAtLeast 'NormalUser)
checkBasicAuthNormal runDB = undefined

checkBasicAuth' :: (forall a. SqlPersistM a -> IO a) -> BasicAuthCheck (AuthUserAtLeast (r :: Role))
checkBasicAuth' runDB = undefined

type AuthContext = BasicAuthCheck (Entity User)

checkBasicAuth :: (forall a. SqlPersistM a -> IO a) -> BasicAuthCheck (Entity User)
checkBasicAuth runDB = BasicAuthCheck $ \BasicAuthData {..} -> runDB $ do
  u <- getBy . UniqueUserName . unpack . decodeUtf8 $ basicAuthUsername
  case u of
    Nothing -> return NoSuchUser
    Just ent -> do
      let valid = validatePassword basicAuthPassword (userPasswordHash . entityVal $ ent)
      if valid then return (Authorized ent) else return BadPassword