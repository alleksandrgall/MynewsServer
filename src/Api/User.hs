{-# HLINT ignore "Use unless" #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Api.User where

import Api.Pagination
import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import DB.Scheme
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Database.Esqueleto.Experimental hiding (get)
import qualified Database.Persist as P
import Dev (createUser, imageRootDev, runDBDev, userIsAdmin_)
import Servant
import Servant.Multipart
import Utils (saveInsertToDbImages, validateImages)

type UserApi =
  BasicAuth "admin" User :> "create" :> MultipartForm Mem (MultipartData Mem) :> Put '[JSON] UserId
    :<|> BasicAuth "admin" User :> "to_author" :> QueryParam' '[Required] "username" String :> PostNoContent
    :<|> GetWithPagination '[JSON] (Entity User)

userApi :: Proxy UserApi
userApi = Proxy

userServer :: Server UserApi
userServer = create :<|> toAuthor :<|> getU

data IncomingUser = IncomingUser {name :: String, password :: String, isAdmin_ :: Bool, isAuthor_ :: Bool}

incUserToDbUser :: IncomingUser -> Maybe ImageId -> IO User
incUserToDbUser IncomingUser {..} imId = createUser name password imId isAdmin_ isAuthor_

instance FromJSON IncomingUser where
  parseJSON (Object o) =
    IncomingUser
      <$> o .: "name"
      <*> o .: "password"
      <*> o .:? "is_admin" .!= False
      <*> o .:? "is_author" .!= False
  parseJSON _ = mempty

instance FromMultipart Mem IncomingUser where
  fromMultipart form = case decode . LBS.fromStrict . encodeUtf8 <$> lookupInput "user" form of
    Left e -> parseIncUserMultipart
    Right Nothing -> parseIncUserMultipart <|> Left "Bad json user data."
    Right (Just u) -> Right u
    where
      parseIncUserMultipart =
        IncomingUser
          <$> (T.unpack <$> lookupInput "name" form)
          <*> (T.unpack <$> lookupInput "password" form)
          <*> withDef (readBool . lookupInput "is_admin" $ form) False
          <*> withDef (readBool . lookupInput "is_author" $ form) False
      readBool :: Either String T.Text -> Maybe Bool
      readBool (Left e) = Nothing
      readBool (Right b)
        | T.map toLower b == "true" = Just True
        | T.map toLower b == "false" = Just False
        | otherwise = Nothing
      withDef (Just x) _ = Right x
      withDef Nothing def = Right def

create :: User -> MultipartData Mem -> Handler UserId
create admin form = do
  userIsAdmin_ admin
  case fromMultipart form of
    Left e -> throwError err400 {errReasonPhrase = e}
    Right incUser -> do
      exists_ <- isUserExists (name incUser)
      when exists_ (throwError err400 {errReasonPhrase = "Username is already taken."})
      maybeAvId <- case lookupFile "avatar" form of
        Left _ -> return Nothing
        Right fd -> do
          v <- validateImages [fd]
          avIds <- liftIO $ saveInsertToDbImages v imageRootDev (const $ return ())
          return (Just . head $ avIds)
      dbU <- liftIO $ incUserToDbUser incUser maybeAvId
      runDBDev $ insert dbU

isUserExists :: String -> Handler Bool
isUserExists name = isJust <$> (runDBDev . getBy $ UniqueUserName name)

toAuthor :: User -> String -> Handler NoContent
toAuthor u name = do
  userIsAdmin_ u
  exists_ <- isUserExists name
  when (not exists_) (throwError err400 {errReasonPhrase = "No such user."})
  runDBDev $ P.updateWhere [UserName P.==. name] [UserIsAuthor P.=. True]
  return NoContent

getU :: Maybe Limit -> Maybe Offset -> Handler (WithOffset [Entity User])
getU lim off = do
  usrs <- runDBDev
    . select
    $ do
      user <- from $ table @User
      addPagination lim off (pure user)
  return $ WithOffset (newOffset lim off) usrs