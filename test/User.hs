{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module User where

import Api.Internal.ImageManager (getImage)
import Api.Internal.Pagination (GetWithPagination, Limit, Offset, WithOffset)
import Api.User (FormatUser, IncomingUserInfo (..), userApi, userServer)
import ClientAuth
import Control.Exception (Exception, throwIO)
import Control.Monad.Catch (throwM)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (ReaderT (runReaderT))
import Crypto.KDF.BCrypt (validatePassword)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Either (fromRight, isRight)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.String (IsString (fromString))
import qualified Data.Text as T
import Data.Time (UTCTime (utctDay), getCurrentTime)
import Database.Persist (Entity)
import qualified Database.Persist as P
import qualified Database.Persist.Sql as P
import Handlers.App (Handler (hDBHandler))
import qualified Handlers.App as A
import Handlers.DB (Handler (hRunDB))
import Handlers.DB.Scheme (EntityField (UserId, UserName), Key (UserKey), User (User, userAvatar, userCreated, userIsAdmin, userIsAuthor, userName, userPasswordHash), UserId)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (hAuthorization)
import qualified Network.Wai.Handler.Warp as Warp
import Servant
import Servant.Client
import Servant.Client.Core (AuthenticatedRequest, Request, basicAuthReq)
import qualified Servant.Client.Core as ClientAuth (AuthClientData)
import Servant.Multipart (FileData (FileData), Input (Input), Mem, MultipartData (MultipartData, files))
import Servant.Multipart.Client
import Test.Hspec
import qualified TestHandler as H
import Utils

withApp :: A.Handler -> (Warp.Port -> IO ()) -> IO ()
withApp h = Warp.testWithApplication (pure $ A.serve_ h userApi userServer)

create :: AuthenticatedRequest (AuthProtect "admin") -> (LBS.ByteString, MultipartData Mem) -> ClientM UserId
toAuthor :: AuthenticatedRequest (AuthProtect "admin") -> String -> ClientM NoContent
getU :: Maybe Limit -> Maybe Offset -> ClientM (WithOffset FormatUser)
(create :<|> toAuthor :<|> getU) = client userApi

multipartIncomingUserInfo :: IncomingUserInfo -> MultipartData Mem
multipartIncomingUserInfo IncomingUserInfo {..} =
  MultipartData
    ( [ Input "name" $ T.pack incomingName,
        Input "password" $ T.pack incomingPassword
      ]
        <> maybe [] (\isAdmin_ -> [Input "is_admin" (T.pack . show $ isAdmin_)]) incomingIsAdmin
        <> maybe [] (\isAuthor_ -> [Input "is_author" (T.pack . show $ isAuthor_)]) incomingIsAuthor
    )
    []

compareSentCreatedUsers :: A.Handler -> IncomingUserInfo -> Maybe (String, FilePath) -> User -> Expectation
compareSentCreatedUsers h incUI incA createdUser = do
  userName createdUser `shouldBe` incomingName incUI
  validatePassword (fromString . incomingPassword $ incUI :: BS.ByteString) (userPasswordHash createdUser) `shouldBe` True
  userIsAdmin createdUser `shouldBe` fromMaybe False (incomingIsAdmin incUI)
  userIsAuthor createdUser `shouldBe` fromMaybe False (incomingIsAuthor incUI)
  date <- utctDay <$> getCurrentTime
  userCreated createdUser `shouldBe` date
  case userAvatar createdUser of
    Nothing -> incA `shouldBe` Nothing
    (Just imId) -> do
      (createdMime, createdAvatar) <-
        runExceptT (flip runReaderT h . A.unApp . getImage $ imId)
          >>= shouldBeRightOr "Image id was returned but no image in the database"
      (incMime, incFp) <- shouldBeJustOr "No avatar was sent, but id was written to user" incA
      incAvatar <- BS.readFile incFp
      (createdMime, createdAvatar) `shouldBe` (incMime, incAvatar)

clearUsers :: A.Handler -> IO ()
clearUsers h = hRunDB (hDBHandler h) (P.deleteWhere [UserName P.!=. "admin"])

userSpec :: A.Handler -> Spec
userSpec h = around (withApp h) $ do
  baseUrl <- runIO $ parseBaseUrl "http://localhost"
  manager <- runIO $ newManager defaultManagerSettings
  let clientEnv port = mkClientEnv manager (baseUrl {baseUrlPort = port})

  describe "PUT /user/create" $
    context "Correct auth" $ do
      let authInfo = authenticateAdmin ("admin", "admin")
      context "Correct userform" $ do
        context "Complete correct form" $
          it "Successfully responds with the correct user id written in the database" $ \port -> do
            let incomingUser =
                  IncomingUserInfo
                    { incomingName = "MyUsername",
                      incomingPassword = "MyPassword",
                      incomingIsAdmin = Just True,
                      incomingIsAuthor = Just True
                    }
                incomingUserForm = multipartIncomingUserInfo incomingUser
            sep <- genBoundary
            createdId <-
              runClientM (create authInfo (sep, incomingUserForm)) (clientEnv port)
                >>= shouldBeRightOr "Client error while making a request"
            createdUser <-
              hRunDB (hDBHandler h) (P.get createdId)
                >>= shouldBeJustOr "User was not created, but id was still returned"
            compareSentCreatedUsers h incomingUser Nothing createdUser >> clearUsers h
        context "Partial correct form" $
          it "Successfully responds with the correct user id written in the database and ads defaults for missing fields" $ \port -> do
            let incomingUser =
                  IncomingUserInfo
                    { incomingName = "MyUsername",
                      incomingPassword = "MyPassword",
                      incomingIsAdmin = Nothing,
                      incomingIsAuthor = Nothing
                    }
                incomingUserForm = multipartIncomingUserInfo incomingUser
            sep <- genBoundary
            createdId <-
              runClientM (create authInfo (sep, incomingUserForm)) (clientEnv port)
                >>= shouldBeRightOr "Client error while making a request"
            createdUser <-
              hRunDB (hDBHandler h) (P.get createdId)
                >>= shouldBeJustOr "User was not created, but id was still returned"
            compareSentCreatedUsers h incomingUser Nothing createdUser >> clearUsers h
        context "Avatar correct name" $
          it "Successfully responds with the correct user id written in the database, and saves an avatar" $ \port -> do
            fileBytes <- LBS.readFile "test/testImages/maxresdefault.jpg"
            let incomingUser =
                  IncomingUserInfo
                    { incomingName = "MyUsername",
                      incomingPassword = "MyPassword",
                      incomingIsAdmin = Just True,
                      incomingIsAuthor = Nothing
                    }
                incomingUserForm = multipartIncomingUserInfo incomingUser
                fd = FileData "avatar" "test/testImages/maxresdefault.jpg" "image/jpg" fileBytes :: FileData Mem
                incomingUserForm' = incomingUserForm {files = [fd]}
            sep <- genBoundary
            createdId <-
              runClientM (create authInfo (sep, incomingUserForm')) (clientEnv port)
                >>= shouldBeRightOr "Client error while making a request"
            createdUser <-
              hRunDB (hDBHandler h) (P.get createdId)
                >>= shouldBeJustOr "User was not created, but id was still returned"
            compareSentCreatedUsers h incomingUser (Just ("image/jpg", "test/testImages/maxresdefault.jpg")) createdUser >> clearUsers h
