{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Api.User where

import Api.Internal.Auth
import Api.Internal.ImageManager
import Api.Internal.Pagination
import App (App, Auth (Auth), askPaginationLimit, runDB)
import Control.Applicative ((<|>))
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Crypto.KDF.BCrypt (hashPassword)
import DB.Scheme
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime (utctDay), getCurrentTime)
import Database.Esqueleto.Experimental hiding (get)
import qualified Database.Persist as P
import GHC.Generics (Generic)
import Katip (Severity (InfoS), katipAddContext, logFM, sl)
import Servant
import Servant.Multipart
import qualified Text.Read as T

type UserApi =
  AuthProtect "admin" :> "create" :> MultipartForm Mem (MultipartData Mem) :> Put '[JSON] UserId
    :<|> AuthProtect "admin" :> "to_author" :> QueryParam' '[Required] "username" String :> PostNoContent
    :<|> GetWithPagination '[JSON] (Entity User)

userApi :: Proxy UserApi
userApi = Proxy

userServer :: ServerT UserApi App
userServer = create :<|> toAuthor :<|> getU

data IncomingUser = IncomingUser {incomingName :: String, incomingPassword :: String, incomingIsAdmin :: Bool, incomingIsAuthor :: Bool}
  deriving (Show, Generic)

incUserToDbUser :: IncomingUser -> Maybe ImageId -> IO User
incUserToDbUser IncomingUser {..} imId = do
  pswdH <- (hashPassword 6 $ encodeUtf8 . T.pack $ incomingPassword :: IO BS.ByteString)
  date <- utctDay <$> getCurrentTime
  return
    User
      { userName = incomingName,
        userAvatar = imId,
        userPasswordHash = pswdH,
        userCreated = date,
        userIsAdmin = incomingIsAdmin,
        userIsAuthor = incomingIsAuthor
      }

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

create :: Auth a -> MultipartData Mem -> App UserId
create (Auth u) form = do
  logFM InfoS "Creating a user"
  userIsAdmin_ u
  case fromMultipart form of
    Left e -> throwError err400 {errReasonPhrase = e}
    Right incUser -> do
      exists_ <- isUserExists (incomingName incUser)
      when exists_ (throwError err400 {errReasonPhrase = "Username is already taken."})
      maybeAvId <- case lookupFile "avatar" form of
        Left _ -> return Nothing
        Right fd -> do
          avIds <- saveAndInsertImages [fd] (const $ return ())
          return (Just . head $ avIds)
      dbU <- liftIO $ incUserToDbUser incUser maybeAvId
      uId <- runDB $ insert dbU
      katipAddContext (sl "user_id" uId) $ logFM InfoS "User created" >> return uId

isUserExists :: String -> App Bool
isUserExists name = isJust <$> (runDB . getBy $ UniqueUserName name)

toAuthor :: Auth a -> String -> App NoContent
toAuthor (Auth u) name = do
  userIsAdmin_ u
  exists_ <- isUserExists name
  unless exists_ (throwError err400 {errReasonPhrase = "No such user."})
  runDB $ P.updateWhere [UserName P.==. name] [UserIsAuthor P.=. True]
  katipAddContext (sl "user_name" name) $ logFM InfoS "User promoted to author" >> return NoContent

getU :: Maybe Limit -> Maybe Offset -> App (WithOffset (Entity User))
getU lim off = do
  katipAddContext (sl "limit" lim <> sl "offset" off) $ do
    logFM InfoS "Sending users"
    maxLimit <- askPaginationLimit
    users <-
      runDB
        . selectPagination lim off maxLimit
        $ from $ table @User
    katipAddContext (sl "user_number" (length users)) $ logFM InfoS "Users sent" >> return users
