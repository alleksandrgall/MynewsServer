{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}

module Category where

import Api.Category (Parent (Parent), categoryApi, categoryServer)
import Api.Internal.Pagination (Limit (Limit), Offset (Offset), WithOffset (..))
import ClientAuth (authenticateAdmin)
import Control.Exception (throwIO)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.List (sort)
import qualified Database.Persist as P
import qualified Database.Persist.Sql as P
import qualified Handlers.App as A
import Handlers.DB (Handler (hRunDB))
import Handlers.DB.Scheme (Category (..), CategoryId, Key (CategoryKey), Unique (UniqueUserName))
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (Status (statusCode))
import Network.Wai.Handler.Warp (Port)
import Servant (AuthProtect, type (:<|>) ((:<|>)))
import Servant.Client (BaseUrl (baseUrlPort), ClientEnv, ClientM, client, mkClientEnv, parseBaseUrl, runClientM)
import Servant.Client.Core (AuthenticatedRequest)
import Test.Hspec (Expectation, Spec, SpecWith, around, before, context, describe, it, runIO, shouldBe)
import Utils (clearCategories, putTestCategoryTreeAndReturn, respondsWithErr, shouldBeJustOr, shouldBeRightOr, withApp)

create :: AuthenticatedRequest (AuthProtect "admin") -> String -> Parent -> ClientM CategoryId
alter :: AuthenticatedRequest (AuthProtect "admin") -> CategoryId -> Maybe String -> Maybe Parent -> ClientM (P.Entity Category)
getC :: Maybe Limit -> Maybe Offset -> ClientM (WithOffset (P.Entity Category))
(create :<|> alter :<|> getC) = client categoryApi

categorySpec :: A.Handler -> Spec
categorySpec h = before (clearCategories h) $
  around (withApp h categoryApi categoryServer) $ do
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl {baseUrlPort = port})

    categoryCreate h clientEnv
    categoryAlter h clientEnv
    categoryGetC h clientEnv

categoryCreate :: A.Handler -> (Port -> ClientEnv) -> SpecWith Port
categoryCreate h clientEnv =
  describe "PUT category/create" $ do
    context "Correct auth" $ do
      let authInfo = authenticateAdmin ("admin", "admin")

      context "Unique category" $ do
        it "creates new category" $ \port -> do
          createdCategId <-
            runClientM (create authInfo "newCateg" (Parent Nothing)) (clientEnv port)
              >>= shouldBeRightOr "Req error, details"
          created <-
            hRunDB (A.hDBHandler h) (P.get createdCategId)
              >>= shouldBeJustOr "Category was not created but id was returned"
          compareExprectedAndCreated created "newCateg" (Parent Nothing)
          createdChildCategId <-
            runClientM (create authInfo "newChildCateg" (Parent (Just createdCategId))) (clientEnv port)
              >>= shouldBeRightOr "Req error, details"
          createdChild <-
            hRunDB (A.hDBHandler h) (P.get createdChildCategId)
              >>= shouldBeJustOr "Category was not created but id was returned"
          compareExprectedAndCreated createdChild "newChildCateg" (Parent (Just createdCategId))

      context "Not a unique category" $ do
        it "responds with 400" $ \port -> do
          newCateg <- hRunDB (A.hDBHandler h) (P.insert $ Category "new Categ" Nothing)
          runClientM (create authInfo "new Categ" (Parent Nothing)) (clientEnv port)
            >>= respondsWithErr 400
          newChildCateg <- hRunDB (A.hDBHandler h) (P.insert $ Category "new Child Categ" (Just newCateg))
          runClientM (create authInfo "new Child Categ" (Parent (Just newCateg))) (clientEnv port)
            >>= respondsWithErr 400

      context "Parent doesn't exist" $ do
        it "responds with 400" $ \port -> do
          runClientM (create authInfo "new Categ" (Parent (Just (CategoryKey . P.SqlBackendKey $ 10)))) (clientEnv port)
            >>= respondsWithErr 400

    context "Incorrect auth" $ do
      let authInfo = authenticateAdmin ("notAdmin", "notAdmin")

      it "responds with 404" $ \port -> do
        runClientM (create authInfo "new Categ" (Parent Nothing)) (clientEnv port)
          >>= respondsWithErr 404

categoryAlter :: A.Handler -> (Port -> ClientEnv) -> SpecWith Port
categoryAlter h clientEnv =
  describe "POST category/alter" $ do
    context "Correct auth" $ do
      let authInfo = authenticateAdmin ("admin", "admin")

      context "Nothing to update provided" $ do
        it "responds with 400" $ \port -> do
          newCateg <- hRunDB (A.hDBHandler h) (P.insert $ Category "New name" Nothing)
          runClientM (alter authInfo newCateg Nothing Nothing) (clientEnv port)
            >>= respondsWithErr 400

      context "Non existing category id provided" $ do
        it "responds with 400" $ \port -> do
          runClientM (alter authInfo (CategoryKey . P.SqlBackendKey $ 1) Nothing Nothing) (clientEnv port)
            >>= respondsWithErr 400

      context "After updates category is not unique" $ do
        it "responds with 400" $ \port -> do
          newCateg <- hRunDB (A.hDBHandler h) (P.insert $ Category "New name" Nothing)
          newChildCateg <- hRunDB (A.hDBHandler h) (P.insert $ Category "New name" (Just newCateg))
          runClientM (alter authInfo newChildCateg Nothing Nothing) (clientEnv port)
            >>= respondsWithErr 400

      context "Some correct updates provided with valid category id" $ do
        it "alters the category" $ \port -> do
          newCateg <- hRunDB (A.hDBHandler h) (P.insert $ Category "New name" Nothing)
          newChildCateg <- hRunDB (A.hDBHandler h) (P.insert $ Category "New Child name" (Just newCateg))
          toBeAlteredCateg <- hRunDB (A.hDBHandler h) (P.insert $ Category "To be altered name" Nothing)
          alteredCateg <-
            runClientM (alter authInfo toBeAlteredCateg (Just "Altered Name") (Just . Parent . Just $ newChildCateg)) (clientEnv port)
              >>= shouldBeRightOr "Server or client error, details"
          compareExprectedAndCreated (P.entityVal alteredCateg) "Altered Name" (Parent (Just newChildCateg))

    context "Incorrect auth" $ do
      let authInfo = authenticateAdmin ("NotAdmin", "NotAdmin")
      it "reponds with 404" $ \port -> do
        newCateg <- hRunDB (A.hDBHandler h) (P.insert $ Category "New name" Nothing)
        newChildCateg <- hRunDB (A.hDBHandler h) (P.insert $ Category "New Child name" (Just newCateg))
        toBeAlteredCateg <- hRunDB (A.hDBHandler h) (P.insert $ Category "To be altered name" Nothing)
        runClientM (alter authInfo toBeAlteredCateg (Just "Altered Name") (Just . Parent . Just $ newChildCateg)) (clientEnv port)
          >>= respondsWithErr 404

categoryGetC :: A.Handler -> (Port -> ClientEnv) -> SpecWith Port
categoryGetC h clientEnv =
  describe "GET category/get" $ do
    it "sorts the list by category id in the ascending order" $ \port -> do
      testCats <- putTestCategoryTreeAndReturn h
      response <-
        runClientM (getC Nothing Nothing) (clientEnv port)
          >>= shouldBeRightOr "Client error while making a request"
      defPaginationLim <-
        runExceptT (flip runReaderT h . A.unApp $ A.askPaginationLimit)
          >>= shouldBeRightOr "Can't get default pagination limit"
      let responseShouldBe = sort $ map P.entityKey testCats
      map P.entityKey (content response) `shouldBe` take (fromIntegral defPaginationLim) responseShouldBe

    context "no offset, no limit" $ do
      it "responds with the list of categories length of which is equal to default pagination limit" $ \port -> do
        testCats <- putTestCategoryTreeAndReturn h
        response <-
          runClientM (getC Nothing Nothing) (clientEnv port)
            >>= shouldBeRightOr "Client error while making a request"
        defPaginationLim <-
          runExceptT (flip runReaderT h . A.unApp $ A.askPaginationLimit)
            >>= shouldBeRightOr "Can't get default pagination limit"
        content response `shouldBe` take (fromIntegral defPaginationLim) testCats

    context "offset" $ do
      it "responds with the lists of categories length of which is equal to default pagination limit starting with provided offset" $ \port -> do
        testCats <- putTestCategoryTreeAndReturn h
        defPaginationLim <-
          runExceptT (flip runReaderT h . A.unApp $ A.askPaginationLimit)
            >>= shouldBeRightOr "Can't get default pagination limit"
        let offset_ = length testCats - fromIntegral defPaginationLim - 1
        response <-
          runClientM (getC Nothing (Just $ Offset offset_)) (clientEnv port)
            >>= shouldBeRightOr "Client error while making a request"
        content response `shouldBe` (take (fromIntegral defPaginationLim) . drop offset_ $ testCats)

    context "limit" $ do
      context "limit less or equal to default" $ do
        it "responds with the lists of users length of which is equal to provided limit" $ \port -> do
          testCats <- putTestCategoryTreeAndReturn h
          defPaginationLim <-
            runExceptT (flip runReaderT h . A.unApp $ A.askPaginationLimit)
              >>= shouldBeRightOr "Can't get default pagination limit"
          let limit_ = defPaginationLim - 1
          response <-
            runClientM (getC (Just $ Limit limit_) Nothing) (clientEnv port)
              >>= shouldBeRightOr "Client error while making a request"
          content response `shouldBe` take (fromIntegral limit_) testCats

      context "limit greater then default" $ do
        it "responds with the lists of users length of which is equal to default pagination limit" $ \port -> do
          testCats <- putTestCategoryTreeAndReturn h
          defPaginationLim <-
            runExceptT (flip runReaderT h . A.unApp $ A.askPaginationLimit)
              >>= shouldBeRightOr "Can't get default pagination limit"
          let limit_ = defPaginationLim + 5
          response <-
            runClientM (getC (Just $ Limit limit_) Nothing) (clientEnv port)
              >>= shouldBeRightOr "Client error while making a request"
          content response `shouldBe` take (fromIntegral defPaginationLim) testCats

    context "offset, limit" $ do
      it "responds with the lists of users length of which is detemined by provided limit starting with provided offset" $ \port -> do
        testCats <- putTestCategoryTreeAndReturn h
        defPaginationLim <-
          runExceptT (flip runReaderT h . A.unApp $ A.askPaginationLimit)
            >>= shouldBeRightOr "Can't get default pagination limit"
        let limit_ = defPaginationLim - 1
            offset_ = length testCats - fromIntegral defPaginationLim - 1
        response <-
          runClientM (getC (Just $ Limit limit_) (Just $ Offset offset_)) (clientEnv port)
            >>= shouldBeRightOr "Client error while making a request"
        adminUser <-
          hRunDB (A.hDBHandler h) (P.getBy $ UniqueUserName "admin")
            >>= shouldBeJustOr "Critical DB error - no admin user"
        content response `shouldBe` (take (fromIntegral limit_) . drop offset_ $ testCats)

compareExprectedAndCreated :: Category -> String -> Parent -> Expectation
compareExprectedAndCreated created name (Parent mParent) = do
  categoryName created `shouldBe` name
  categoryParent created `shouldBe` mParent