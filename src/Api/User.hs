{-# HLINT ignore "Use unless" #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Api.User where

import Api.Auth
import Api.Pagination
import App
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
import Dev (createUser, runDBDev)
import GHC.Generics (Generic)
import Servant
import Servant.Multipart
import qualified Text.Read as T
import Utils (saveInsertToDbImages, validateImages)

type UserApi =
  BasicAuth "admin" (Entity User) :> "create" :> MultipartForm Mem (MultipartData Mem) :> Put '[JSON] UserId
    :<|> BasicAuth "admin" (Entity User) :> "to_author" :> QueryParam' '[Required] "username" String :> PostNoContent
    :<|> GetWithPagination '[JSON] (Entity User)

userApi :: Proxy UserApi
userApi = Proxy

userServer :: ServerT UserApi App
userServer = create :<|> toAuthor :<|> getU

data IncomingUser = IncomingUser {incomingName :: String, incomingPassword :: String, incomingIsAdmin :: Bool, incomingIsAuthor :: Bool}
  deriving (Show, Generic)

incUserToDbUser :: IncomingUser -> Maybe ImageId -> IO User
incUserToDbUser IncomingUser {..} imId = createUser incomingName incomingPassword imId incomingIsAdmin incomingIsAuthor

instance FromJSON IncomingUser where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 8}

instance FromMultipart Mem IncomingUser where
  fromMultipart form = parseMultipart <|> (lookupInput "user" form >>= eitherDecode . LBS.fromStrict . encodeUtf8)
    where
      parseMultipart =
        IncomingUser <$> (T.unpack <$> lookupInput "name" form)
          <*> (T.unpack <$> lookupInput "password" form)
          <*> withDef (lookupInput "is_admin" form >>= T.readEither . T.unpack) False
          <*> withDef (lookupInput "is_author" form >>= T.readEither . T.unpack) False
      withDef (Right x) _ = Right x
      withDef _ def = Right def

create :: Entity User -> MultipartData Mem -> App UserId
create admin form = do
  userIsAdmin_ admin
  case fromMultipart form of
    Left e -> throwError err400 {errReasonPhrase = e}
    Right incUser -> do
      exists_ <- isUserExists (incomingName incUser)
      when exists_ (throwError err400 {errReasonPhrase = "Username is already taken."})
      maybeAvId <- case lookupFile "avatar" form of
        Left _ -> return Nothing
        Right fd -> do
          v <- validateImages [fd]
          imageRoot <- askImageRoot
          avIds <- liftIO $ saveInsertToDbImages v imageRoot (const $ return ())
          return (Just . head $ avIds)
      dbU <- liftIO $ incUserToDbUser incUser maybeAvId
      runDBDev $ insert dbU

isUserExists :: String -> App Bool
isUserExists name = isJust <$> (runDBDev . getBy $ UniqueUserName name)

toAuthor :: Entity User -> String -> App NoContent
toAuthor u name = do
  userIsAdmin_ u
  exists_ <- isUserExists name
  when (not exists_) (throwError err400 {errReasonPhrase = "No such user."})
  runDBDev $ P.updateWhere [UserName P.==. name] [UserIsAuthor P.=. True]
  return NoContent

getU :: Maybe Limit -> Maybe Offset -> App (WithOffset [Entity User])
getU lim off = do
  runDBDev
    . selectPagination lim off
    $ do
      from $ table @User
