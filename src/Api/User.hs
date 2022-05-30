{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Api.User where

import Api.Internal.Auth (userIsAdmin_)
import Api.Internal.ImageManager (saveAndInsertImages)
import Api.Internal.Pagination
  ( GetWithPagination,
    Limit,
    Offset,
    WithOffset,
    selectPagination,
  )
import App (App, askPaginationLimit, runDB)
import App.Auth (Auth (Auth))
import Control.Applicative ((<|>))
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Crypto.KDF.BCrypt (hashPassword)
import DB.Scheme
  ( EntityField (UserIsAuthor, UserName),
    ImageId,
    Unique (UniqueUserName),
    User (..),
    UserId,
  )
import qualified Data.Aeson as A
import Data.Bool (bool)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (isJust)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime (utctDay), getCurrentTime)
import Database.Esqueleto.Experimental hiding (get)
import qualified Database.Persist as P
import qualified Database.Persist.Sql as P
import GHC.Generics (Generic)
import Katip (Severity (InfoS), katipAddContext, logFM, sl)
import Servant
  ( AuthProtect,
    HasServer (ServerT),
    JSON,
    NoContent (..),
    PostNoContent,
    Proxy (..),
    Put,
    QueryParam',
    Required,
    ServerError (errReasonPhrase),
    err400,
    throwError,
    type (:<|>) (..),
    type (:>),
  )
import Servant.Multipart
  ( FromMultipart (..),
    Mem,
    MultipartData,
    MultipartForm,
    lookupFile,
    lookupInput,
  )
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

instance A.FromJSON IncomingUser where
  parseJSON = A.genericParseJSON A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_' . drop 8}

instance FromMultipart Mem IncomingUser where
  fromMultipart form = parseMultipart <|> (lookupInput "user" form >>= A.eitherDecode . LBS.fromStrict . encodeUtf8)
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
      either (\e -> throwError err400 {errReasonPhrase = e}) return
        =<< ( runDB . runExceptT $ do
                isUserExists $ incomingName incUser
            )
      maybeAvId <- case lookupFile "avatar" form of
        Left _ -> return Nothing
        Right fd -> do
          avIds <- saveAndInsertImages [fd] (const $ return ())
          return (Just . head $ avIds)
      dbU <- liftIO $ incUserToDbUser incUser maybeAvId
      uId <- runDB $ insert dbU
      katipAddContext (sl "user_id" uId) $ logFM InfoS "User created" >> return uId

isUserExists :: String -> ExceptT String P.SqlPersistM ()
isUserExists name = ExceptT $ bool (Left "Username is already taken.") (Right ()) . isJust <$> getBy (UniqueUserName name)

toAuthor :: Auth a -> String -> App NoContent
toAuthor (Auth u) name = do
  userIsAdmin_ u
  either (\e -> throwError err400 {errReasonPhrase = e}) return
    =<< ( runDB . runExceptT $ do
            isUserExists name
        )
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
