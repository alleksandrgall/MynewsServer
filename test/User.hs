{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module User where

import Api.Internal.Pagination (Limit (Limit), Offset (Offset), WithOffset, content)
import Api.User (FormatUser (formatUserId), IncomingUserInfo (..), formatEntityUser, userApi, userServer)
import Control.Exception (throwIO)
import Control.Monad (void)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (runReaderT))
import Crypto.KDF.BCrypt (validatePassword)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.String (IsString (fromString))
import qualified Data.Text as T
import Data.Time (UTCTime (utctDay), getCurrentTime)
import qualified Database.Persist as P
import Handlers.App (Handler (hDBHandler))
import qualified Handlers.App as A
import Handlers.DB (Handler (hRunDB))
import Handlers.DB.Scheme (Unique (UniqueUserName), User (userAvatar, userCreated, userIsAdmin, userIsAuthor, userName, userPasswordHash), UserId)
import qualified Handlers.Image as I
import Internal.ClientAuth (authenticateAdmin)
import Internal.Image.Test (ImageTestIO)
import qualified Internal.Image.Test as I
import Internal.Utils (clearUsers, createTestUser, respondsWithErr, shouldBeJustOr, shouldBeRightOr, withApp)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (Status (statusCode))
import Network.Wai.Handler.Warp (Port)
import qualified Network.Wai.Handler.Warp as Warp
import Servant (AuthProtect, NoContent, type (:<|>) ((:<|>)))
import Servant.Client (BaseUrl (baseUrlPort), ClientEnv, ClientError (FailureResponse), ClientM, ResponseF (responseStatusCode), client, mkClientEnv, parseBaseUrl, runClientM)
import Servant.Client.Core (AuthenticatedRequest)
import Servant.Multipart (FileData (FileData), Input (Input), Mem, MultipartData (MultipartData, files))
import Servant.Multipart.Client (genBoundary)
import Test.Hspec (Expectation, Spec, SpecWith, around, before, context, describe, expectationFailure, it, runIO, shouldBe)

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

userSpec :: A.Handler ImageTestIO -> Spec
userSpec h = before (clearUsers h) $
  around (withApp h userApi userServer) $ do
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl {baseUrlPort = port})

    userCreate h clientEnv
    userToAuthor h clientEnv
    userGetU h clientEnv

userCreate :: A.Handler ImageTestIO -> (Port -> ClientEnv) -> SpecWith Port
userCreate h clientEnv =
  describe "PUT /user/create" $ do
    context "Correct auth" $ do
      let authInfo = authenticateAdmin ("admin", "admin")
      context "Correct userform" $ do
        context "Complete form" $ do
          it "responds with the correct user id written in the database" $ \port -> do
            let incomingUser =
                  IncomingUserInfo
                    { incomingName = "MyUsername",
                      incomingPassword = "MyPassword",
                      incomingIsAdmin = Just True,
                      incomingIsAuthor = Just True
                    }
            created <- sendUser h incomingUser Nothing authInfo clientEnv port
            compareExpectedAndCreated h incomingUser Nothing created

        context "Partial correct form" $ do
          it "responds with the correct user id written in the database and ads defaults for missing fields" $ \port -> do
            let incomingUser =
                  IncomingUserInfo
                    { incomingName = "MyUsername",
                      incomingPassword = "MyPassword",
                      incomingIsAdmin = Nothing,
                      incomingIsAuthor = Nothing
                    }
            created <- sendUser h incomingUser Nothing authInfo clientEnv port
            compareExpectedAndCreated h incomingUser Nothing created

        context "Avatar" $ do
          it "responds with the correct user id written in the database, and saves an avatar" $ \port -> do
            maxImSize <- liftIO . I.unImageTestIO $ I.cMaxImageSize . I.hConfig $ A.hImageHandler h
            let incomingUser =
                  IncomingUserInfo
                    { incomingName = "MyUsername",
                      incomingPassword = "MyPassword",
                      incomingIsAdmin = Just True,
                      incomingIsAuthor = Nothing
                    }
                imContent = BS.take (fromIntegral $ maxImSize - 1) (fromString ['1', '2' ..] :: BS.ByteString)
                fd = FileData "avatar" "test/testImages/image0.jpg" "image/jpg" "imContent" :: FileData Mem
            created <- sendUser h incomingUser (Just fd) authInfo clientEnv port
            compareExpectedAndCreated h incomingUser (Just ("image/jpg", "imContent")) created

        context "Avatar too large" $ do
          it "responds with 413" $ \port -> do
            maxImSize <- liftIO . I.unImageTestIO $ I.cMaxImageSize . I.hConfig $ A.hImageHandler h
            let incomingUser =
                  IncomingUserInfo
                    { incomingName = "MyUsername",
                      incomingPassword = "MyPassword",
                      incomingIsAdmin = Just True,
                      incomingIsAuthor = Nothing
                    }
                imContent = LBS.take (fromIntegral $ maxImSize + 1) (fromString ['1', '2' ..] :: LBS.ByteString)
                fd = FileData "avatar" "test/testImages/image0.jpg" "image/jpg" imContent :: FileData Mem
                incomingUserForm = (multipartIncomingUserInfo incomingUser) {files = [fd]}
            sep <- genBoundary
            runClientM (create authInfo (sep, incomingUserForm)) (clientEnv port) >>= respondsWithErr 413

        context "occupied username" $ do
          it "responds with 400" $ \port -> do
            userWithTheSameName <- createTestUser "MyUsername" "pass" Nothing Nothing False False
            hRunDB (hDBHandler h) (P.insert userWithTheSameName)
            let incomingUser =
                  IncomingUserInfo
                    { incomingName = "MyUsername",
                      incomingPassword = "MyPassword",
                      incomingIsAdmin = Just True,
                      incomingIsAuthor = Nothing
                    }
                incomingUserForm = multipartIncomingUserInfo incomingUser
            sep <- genBoundary
            runClientM (create authInfo (sep, incomingUserForm)) (clientEnv port)
              >>= respondsWithErr 400

      context "Incorrect userform" $ do
        context "Avatar incorrect name" $ do
          it "Ignores the file and responds with the correct user id written in the database" $ \port -> do
            maxImSize <- liftIO . I.unImageTestIO $ I.cMaxImageSize . I.hConfig $ A.hImageHandler h
            let incomingUser =
                  IncomingUserInfo
                    { incomingName = "MyUsername",
                      incomingPassword = "MyPassword",
                      incomingIsAdmin = Just True,
                      incomingIsAuthor = Nothing
                    }
                imContent = LBS.take (fromIntegral $ maxImSize - 1) (fromString ['1', '2' ..] :: LBS.ByteString)
                fd = FileData "NotAvatar" "test/testImages/image0.jpg" "image/jpg" imContent :: FileData Mem
            created <- sendUser h incomingUser (Just fd) authInfo clientEnv port
            compareExpectedAndCreated h incomingUser Nothing created

    context "Incorrect auth" $ do
      let authInfo = authenticateAdmin ("notAdmin", "notAdmin")
      it "Responds with 404" $ \port -> do
        maxImSize <- liftIO . I.unImageTestIO $ I.cMaxImageSize . I.hConfig $ A.hImageHandler h
        let incomingUser =
              IncomingUserInfo
                { incomingName = "MyUsername",
                  incomingPassword = "MyPassword",
                  incomingIsAdmin = Just True,
                  incomingIsAuthor = Nothing
                }
            incomingUserForm = multipartIncomingUserInfo incomingUser
            imContent = LBS.take (fromIntegral $ maxImSize - 1) (fromString ['1', '2' ..] :: LBS.ByteString)
            fd = FileData "avatar" "test/testImages/image0.jpg" "image/jpg" imContent :: FileData Mem
            incomingUserForm' = incomingUserForm {files = [fd]}
        sep <- genBoundary
        runClientM (create authInfo (sep, incomingUserForm')) (clientEnv port)
          >>= respondsWithErr 404

userToAuthor :: A.Handler ImageTestIO -> (Port -> ClientEnv) -> SpecWith Port
userToAuthor h clientEnv =
  describe "POST /user/toAuthor" $ do
    testU <- runIO $ createTestUser "test" "testPass" Nothing Nothing False False

    context "Correct auth" $ do
      let authInfo = authenticateAdmin ("admin", "admin")
      context "Existing username" $ do
        it "promotes user to author" $ \port -> do
          void $ hRunDB (hDBHandler h) $ P.insert testU
          void $ runClientM (toAuthor authInfo "test") (clientEnv port)
          promoted <-
            hRunDB (hDBHandler h) (P.getBy (UniqueUserName "test"))
              >>= shouldBeJustOr "Critical DB error"
          (userIsAuthor . P.entityVal $ promoted) `shouldBe` True

      context "Not existing username" $ do
        it "Responds with 400" $ \port -> do
          void $ hRunDB (hDBHandler h) $ P.insert testU
          runClientM (toAuthor authInfo "incorrectName") (clientEnv port) >>= \case
            Left (FailureResponse _ fResp) ->
              if (statusCode . responseStatusCode $ fResp) == 400
                then pure ()
                else expectationFailure "Server didn't respond with 400"
            Left e -> throwIO e
            _ -> expectationFailure "Server didn't respond with an error"

    context "Incorrect auth" $ do
      let authInfo = authenticateAdmin ("NotAdmin", "NotAdmin")

      it "Responds with 404" $ \port -> do
        void $ hRunDB (hDBHandler h) $ P.insert testU
        runClientM (toAuthor authInfo "test") (clientEnv port) >>= respondsWithErr 404

userGetU :: A.Handler ImageTestIO -> (Port -> ClientEnv) -> SpecWith Port
userGetU h clientEnv =
  describe "GET /user/get" $ do
    testUs <- mapM (\i -> runIO $ createTestUser ("test" ++ show i) ("testPass" ++ show i) Nothing Nothing False False) [1 .. 5 :: Int]

    it "sorts the list by user id in the ascending order" $ \port -> do
      testUIds <- hRunDB (hDBHandler h) $ mapM P.insert testUs
      response <-
        runClientM (getU Nothing Nothing) (clientEnv port)
          >>= shouldBeRightOr "Client error while making a request"
      defPaginationLim <-
        runExceptT (flip runReaderT h . A.unApp $ A.askPaginationLimit)
          >>= shouldBeRightOr "Can't get default pagination limit"
      adminUser <-
        hRunDB (hDBHandler h) (P.getBy $ UniqueUserName "admin")
          >>= shouldBeJustOr "Critical DB error - no admin user"
      let formatUsersOrderIds = sort $ P.entityKey adminUser : testUIds
      map formatUserId (content response) `shouldBe` take (fromIntegral defPaginationLim) formatUsersOrderIds

    context "no offset, no limit" $ do
      it "responds with the lists of users length of which is equal to default pagination limit" $ \port -> do
        testUIds <- hRunDB (hDBHandler h) $ mapM P.insert testUs
        response <-
          runClientM (getU Nothing Nothing) (clientEnv port)
            >>= shouldBeRightOr "Client error while making a request"
        defPaginationLim <-
          runExceptT (flip runReaderT h . A.unApp $ A.askPaginationLimit)
            >>= shouldBeRightOr "Can't get default pagination limit"
        adminUser <-
          hRunDB (hDBHandler h) (P.getBy $ UniqueUserName "admin")
            >>= shouldBeJustOr "Critical DB error - no admin user"
        let formatUsersShouldBe = formatEntityUser adminUser : zipWith (\testUId testU -> formatEntityUser $ P.Entity testUId testU) testUIds testUs
        content response `shouldBe` take (fromIntegral defPaginationLim) formatUsersShouldBe

    context "offset" $ do
      it "responds with the lists of users length of which is equal to default pagination limit starting with provided offset" $ \port -> do
        testUIds <- hRunDB (hDBHandler h) $ mapM P.insert testUs
        defPaginationLim <-
          runExceptT (flip runReaderT h . A.unApp $ A.askPaginationLimit)
            >>= shouldBeRightOr "Can't get default pagination limit"
        let offset_ = length testUIds - fromIntegral defPaginationLim
        response <-
          runClientM (getU Nothing (Just $ Offset offset_)) (clientEnv port)
            >>= shouldBeRightOr "Client error while making a request"
        adminUser <-
          hRunDB (hDBHandler h) (P.getBy $ UniqueUserName "admin")
            >>= shouldBeJustOr "Critical DB error - no admin user"
        let formatUsersShouldBe = formatEntityUser adminUser : zipWith (\testUId testU -> formatEntityUser $ P.Entity testUId testU) testUIds testUs
        content response `shouldBe` (take (fromIntegral defPaginationLim) . drop offset_ $ formatUsersShouldBe)

    context "limit" $ do
      context "limit less or equal to default" $ do
        it "responds with the lists of users length of which is equal to provided limit" $ \port -> do
          testUIds <- hRunDB (hDBHandler h) $ mapM P.insert testUs
          defPaginationLim <-
            runExceptT (flip runReaderT h . A.unApp $ A.askPaginationLimit)
              >>= shouldBeRightOr "Can't get default pagination limit"
          let limit_ = defPaginationLim - 1
          response <-
            runClientM (getU (Just $ Limit limit_) Nothing) (clientEnv port)
              >>= shouldBeRightOr "Client error while making a request"
          adminUser <-
            hRunDB (hDBHandler h) (P.getBy $ UniqueUserName "admin")
              >>= shouldBeJustOr "Critical DB error - no admin user"
          let formatUsersShouldBe = formatEntityUser adminUser : zipWith (\testUId testU -> formatEntityUser $ P.Entity testUId testU) testUIds testUs
          content response `shouldBe` take (fromIntegral limit_) formatUsersShouldBe

      context "limit greater then default" $ do
        it "responds with the lists of users length of which is equal to default pagination limit" $ \port -> do
          testUIds <- hRunDB (hDBHandler h) $ mapM P.insert testUs
          defPaginationLim <-
            runExceptT (flip runReaderT h . A.unApp $ A.askPaginationLimit)
              >>= shouldBeRightOr "Can't get default pagination limit"
          let limit_ = defPaginationLim + 5
          response <-
            runClientM (getU (Just $ Limit limit_) Nothing) (clientEnv port)
              >>= shouldBeRightOr "Client error while making a request"
          adminUser <-
            hRunDB (hDBHandler h) (P.getBy $ UniqueUserName "admin")
              >>= shouldBeJustOr "Critical DB error - no admin user"
          let formatUsersShouldBe = formatEntityUser adminUser : zipWith (\testUId testU -> formatEntityUser $ P.Entity testUId testU) testUIds testUs
          content response `shouldBe` take (fromIntegral defPaginationLim) formatUsersShouldBe

    context "offset, limit" $ do
      it "responds with the lists of users length of which is detemined by provided limit starting with provided offset" $ \port -> do
        testUIds <- hRunDB (hDBHandler h) $ mapM P.insert testUs
        defPaginationLim <-
          runExceptT (flip runReaderT h . A.unApp $ A.askPaginationLimit)
            >>= shouldBeRightOr "Can't get default pagination limit"
        let limit_ = defPaginationLim - 1
            offset_ = length testUIds - fromIntegral defPaginationLim - 1
        response <-
          runClientM (getU (Just $ Limit limit_) (Just $ Offset offset_)) (clientEnv port)
            >>= shouldBeRightOr "Client error while making a request"
        adminUser <-
          hRunDB (hDBHandler h) (P.getBy $ UniqueUserName "admin")
            >>= shouldBeJustOr "Critical DB error - no admin user"
        let formatUsersShouldBe = formatEntityUser adminUser : zipWith (\testUId testU -> formatEntityUser $ P.Entity testUId testU) testUIds testUs
        content response `shouldBe` (take (fromIntegral limit_) . drop offset_ $ formatUsersShouldBe)

compareExpectedAndCreated :: A.Handler ImageTestIO -> IncomingUserInfo -> Maybe (String, BS.ByteString) -> User -> Expectation
compareExpectedAndCreated h incUI incA createdUser = do
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
        (I.runImage (A.hImageHandler h) . I.getImageData $ imId)
          >>= shouldBeJustOr "Image id was returned but no image in the database"
      (incMime, incContent) <- shouldBeJustOr "No avatar was sent, but id was written to user" incA
      (createdMime, createdAvatar) `shouldBe` (incMime, incContent)

sendUser ::
  A.Handler ImageTestIO ->
  IncomingUserInfo ->
  Maybe (FileData Mem) ->
  AuthenticatedRequest (AuthProtect "admin") ->
  (Port -> ClientEnv) ->
  Port ->
  IO User
sendUser h incomingUser fd authInfo clientEnv port = do
  let incomingUserForm = multipartIncomingUserInfo incomingUser
      incomingUserForm' = case fd of
        (Just fd') -> incomingUserForm {files = [fd']}
        Nothing -> incomingUserForm
  sep <- genBoundary
  createdId <-
    runClientM (create authInfo (sep, incomingUserForm')) (clientEnv port)
      >>= shouldBeRightOr "Client error while making a request"
  hRunDB (hDBHandler h) (P.get createdId)
    >>= shouldBeJustOr "User was not created, but id was still returned"