{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Api.User where

import Api.Internal.Auth (userIsAdmin_)
import Api.Internal.Pagination
  ( GetWithPagination,
    Limit,
    Offset,
    WithOffset,
    selectPagination,
  )
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (asks)
import Crypto.KDF.BCrypt (hashPassword)
import Data.Aeson (FromJSON, Options (fieldLabelModifier), ToJSON, camelTo2, defaultOptions, genericParseJSON, genericToJSON)
import qualified Data.Aeson as A
import Data.Bool (bool)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime (utctDay), getCurrentTime)
import Data.Time.Calendar (Day)
import Database.Esqueleto.Experimental
  ( Entity,
    PersistStoreWrite (insert),
    PersistUniqueRead (getBy),
    asc,
    from,
    orderBy,
    table,
    (^.),
  )
import qualified Database.Persist as P
import qualified Database.Persist.Sql as P
import GHC.Generics (Generic)
import Handlers.App (App, Auth (..), Handler (hImageHandler), askPaginationLimit, runDB)
import Handlers.App.HandleExcept (handleFormatExcept, handlePutException)
import Handlers.DB.Scheme
  ( EntityField (UserId, UserIsAuthor, UserName),
    ImageId,
    Unique (UniqueUserName),
    User (..),
    UserId,
  )
import qualified Handlers.Image as I
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
  (AuthProtect "admin" :> "create" :> MultipartForm Mem (MultipartData Mem) :> Put '[JSON] UserId)
    :<|> (AuthProtect "admin" :> "to_author" :> QueryParam' '[Required] "username" String :> PostNoContent)
    :<|> GetWithPagination '[JSON] FormatUser

userApi :: Proxy UserApi
userApi = Proxy

userServer :: (MonadMask imageM, MonadIO imageM) => ServerT UserApi (App imageM)
userServer = create :<|> toAuthor :<|> getU

data IncomingUserInfo = IncomingUserInfo
  { incomingName :: String,
    incomingPassword :: String,
    incomingIsAdmin :: Maybe Bool,
    incomingIsAuthor :: Maybe Bool
  }
  deriving (Show, Generic, Eq)

incUserInfoToDbUser :: IncomingUserInfo -> Maybe ImageId -> IO User
incUserInfoToDbUser IncomingUserInfo {..} imId = do
  pswdH <- (hashPassword 6 $ encodeUtf8 . T.pack $ incomingPassword :: IO BS.ByteString)
  date <- utctDay <$> getCurrentTime
  return
    User
      { userName = incomingName,
        userAvatar = imId,
        userPasswordHash = pswdH,
        userCreated = date,
        userIsAdmin = fromMaybe False incomingIsAdmin,
        userIsAuthor = fromMaybe False incomingIsAuthor
      }

instance A.FromJSON IncomingUserInfo where
  parseJSON = A.genericParseJSON A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_' . drop 8}

instance A.ToJSON IncomingUserInfo where
  toJSON = A.genericToJSON A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_' . drop 8}

instance FromMultipart Mem IncomingUserInfo where
  fromMultipart form = parseMultipart
    where
      parseMultipart =
        IncomingUserInfo <$> (T.unpack <$> lookupInput "name" form)
          <*> (T.unpack <$> lookupInput "password" form)
          <*> either (\_ -> Right Nothing) (Right . Just) (lookupInput "is_admin" form >>= T.readEither . T.unpack)
          <*> either (\_ -> Right Nothing) (Right . Just) (lookupInput "is_author" form >>= T.readEither . T.unpack)

create :: (MonadMask imageM, MonadIO imageM) => Auth a -> MultipartData Mem -> App imageM UserId
create (Auth u) form = do
  logFM InfoS "Creating a user"
  userIsAdmin_ u
  case fromMultipart form of
    Left e -> throwError err400 {errReasonPhrase = e}
    Right incUser -> do
      either (\e -> throwError err400 {errReasonPhrase = e}) return
        =<< ( runDB . runExceptT $ do
                usernameFree $ incomingName incUser
            )
      maybeAvId <- case lookupFile "avatar" form of
        Left _ -> return Nothing
        Right fd -> do
          imH <- asks hImageHandler
          (avId :| _) <- handlePutException . handleFormatExcept $ I.runImage imH $ I.saveImages (const $ return ()) (fd :| [])
          return (Just avId)
      dbU <- liftIO $ incUserInfoToDbUser incUser maybeAvId
      uId <- runDB $ insert dbU
      katipAddContext (sl "user_id" uId) $ logFM InfoS "User created" >> return uId

usernameFree :: String -> ExceptT String P.SqlPersistM ()
usernameFree name = ExceptT $ bool (Right ()) (Left "Username is already taken") . isJust <$> getBy (UniqueUserName name)

userExists :: String -> ExceptT String P.SqlPersistM ()
userExists name = ExceptT $ bool (Left "User does not exists.") (Right ()) . isJust <$> getBy (UniqueUserName name)

toAuthor :: Auth a -> String -> App imageM NoContent
toAuthor (Auth u) name = do
  userIsAdmin_ u
  either (\e -> throwError err400 {errReasonPhrase = e}) return
    =<< ( runDB . runExceptT $ do
            userExists name
        )
  runDB $ P.updateWhere [UserName P.==. name] [UserIsAuthor P.=. True]
  katipAddContext (sl "user_name" name) $ logFM InfoS "User promoted to author" >> return NoContent

data FormatUser = FormatUser
  { formatUserId :: UserId,
    formatUserUsername :: String,
    formatUserAvatar :: Maybe ImageId,
    formatUserCreated :: Day,
    formatUserIsAdmin :: Bool,
    formatUserIsAuthor :: Bool
  }
  deriving (Show, Generic, Eq)

instance ToJSON FormatUser where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 11}

instance FromJSON FormatUser where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 11}

formatEntityUser :: Entity User -> FormatUser
formatEntityUser entUser =
  FormatUser
    { formatUserId = P.entityKey entUser,
      formatUserUsername = userName . P.entityVal $ entUser,
      formatUserAvatar = userAvatar . P.entityVal $ entUser,
      formatUserCreated = userCreated . P.entityVal $ entUser,
      formatUserIsAdmin = userIsAdmin . P.entityVal $ entUser,
      formatUserIsAuthor = userIsAuthor . P.entityVal $ entUser
    }

getU :: Maybe Limit -> Maybe Offset -> App imageM (WithOffset FormatUser)
getU lim off = do
  katipAddContext (sl "limit" lim <> sl "offset" off) $ do
    logFM InfoS "Sending users"
    maxLimit <- askPaginationLimit
    users <-
      runDB
        . selectPagination lim off maxLimit
        $ do
          u <- from $ table @User
          orderBy [asc (u ^. UserId)]
          pure u
    katipAddContext (sl "user_number" (length users)) $ logFM InfoS "Users sent" >> return (formatEntityUser <$> users)
