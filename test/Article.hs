module Article where

import Api.Article (IncomingArticle (..), articleApi, articleServer)
import Api.Article.Filters (SortBy (Author, Category_, Date, ImageNum))
import Api.Article.Get
  ( FormatArticle (..),
    getFormatArticle,
    parseListToNest,
  )
import Api.Internal.Optional (MaybeSetter (MaybeSetter))
import Api.Internal.Pagination (Limit (Limit), Offset (Offset), WithOffset (content))
import Api.User (FormatUser (formatUserUsername), formatEntityUser)
import Control.Monad (void, zipWithM, zipWithM_)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import qualified Data.ByteString.Lazy as LBS
import Data.List (isPrefixOf, sortBy, tails)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromJust, fromMaybe)
import Data.String (IsString (fromString))
import qualified Data.Text as T
import Data.Time (Day (ModifiedJulianDay, toModifiedJulianDay), UTCTime (utctDay), getCurrentTime)
import qualified Database.Persist as P
import qualified Database.Persist.Sql as P
import qualified Handlers.App as A
import Handlers.DB (Handler (hRunDB))
import Handlers.DB.Scheme
  ( Article (..),
    ArticleId,
    Category (Category, categoryParent),
    CategoryId,
    EntityField (ImageArticleArticleId, ImageId),
    Image (Image, imageCt, imagePath),
    ImageArticle (ImageArticle, imageArticleImageId),
    ImageId,
    Key (ArticleKey, unCategoryKey),
    Unique (UniqueUserName),
    User (userName),
    UserId,
  )
import Handlers.Image (DeleteStatus (DeleteStatus), Handler (hDBHandler))
import qualified Handlers.Image as I
import Internal.ClientAuth (authenticateNormal)
import qualified Internal.Image.Test as I
import Internal.Utils (clearArticles, createTestUser, getNestId, getNestName, respondsWithErr, shouldBeJustOr, shouldBeRightOr, withApp)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (Port)
import Servant (AuthProtect, type (:<|>) ((:<|>)))
import Servant.Client (BaseUrl (baseUrlPort), ClientEnv, ClientM, client, mkClientEnv, parseBaseUrl, runClientM)
import Servant.Client.Core (AuthenticatedRequest)
import Servant.Multipart (FileData (FileData, fdFileCType, fdPayload), Input (Input), Mem, MultipartData (MultipartData))
import Servant.Multipart.Client (genBoundary)
import Test.Hspec
  ( Expectation,
    Spec,
    SpecWith,
    around,
    before,
    context,
    describe,
    it,
    runIO,
    shouldBe,
  )

create :: AuthenticatedRequest (AuthProtect "normal") -> (LBS.ByteString, MultipartData Mem) -> ClientM FormatArticle
alterAdd :: AuthenticatedRequest (AuthProtect "normal") -> ArticleId -> (LBS.ByteString, MultipartData Mem) -> ClientM FormatArticle
alterDelete :: AuthenticatedRequest (AuthProtect "normal") -> ArticleId -> [ImageId] -> ClientM DeleteStatus
getA ::
  Maybe Day -> -- created_since
  Maybe Day -> -- created_until
  Maybe Day -> -- created_at
  Maybe String -> -- author_name
  Maybe CategoryId -> -- category
  Maybe String -> -- title_has
  Maybe String -> -- content_has
  Maybe String -> -- search
  Maybe SortBy -> -- sortParam
  Maybe Limit ->
  Maybe Offset ->
  ClientM (WithOffset FormatArticle)
(create :<|> alterAdd :<|> alterDelete :<|> getA) = client articleApi

articleSpec :: A.Handler I.ImageTestIO -> Spec
articleSpec h = before (clearArticles h) $
  around (withApp h articleApi articleServer) $ do
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl {baseUrlPort = port})

    articleCreate h clientEnv
    articleAlterAdd h clientEnv
    articleAlterDelete h clientEnv
    articleGetA h clientEnv

multipartIncomingArticle :: IncomingArticle -> [FileData Mem] -> MultipartData Mem
multipartIncomingArticle IncomingArticle {..} =
  MultipartData $
    [ Input "title" (T.pack incomingTitle),
      Input "content" (T.pack incomingContent),
      Input "category_id" $ T.pack . show . P.unSqlBackendKey . unCategoryKey $ incomingCategoryId
    ]
      <> maybe [] (\isPublised -> [Input "is_published" $ T.pack . show $ isPublised]) incomingIsPublished

articleCreate :: A.Handler I.ImageTestIO -> (Port -> ClientEnv) -> SpecWith Port
articleCreate h clientEnv =
  describe "PUT article/create/" $ do
    author <- runIO $ createTestUser "Author" "AuthorPass" Nothing Nothing False True
    notAnAuthor <- runIO $ createTestUser "NotAnAuthor" "NotAnAuthorPass" Nothing Nothing False False
    notAuthor <- runIO $ createTestUser "NotAnAuthor" "NotAnAuthorPass" Nothing Nothing False True
    let image1 = FileData "someImName" "" "image/jpeg" "imContent1" :: FileData Mem
        image2 = FileData "veryRandomName" "" "image/png" "imContent2" :: FileData Mem
        image3 = FileData "thirdImage" "" "image/webm" "imContent3" :: FileData Mem
    maxImSize <- runIO $ liftIO . I.unImageTestIO $ I.cMaxImageSize . I.hConfig $ A.hImageHandler h
    let largeContent = LBS.take (fromIntegral $ maxImSize + 1) (fromString ['1', '2' ..] :: LBS.ByteString)
        tooLargeImage = FileData "largeImage" "" "image/png" largeContent :: FileData Mem
    let notAnImage = FileData "notAnImage" "" "notAnImageCT" "imContent" :: FileData Mem
    let articleCategParent = Category "Parent art categ" Nothing
        articleCateg = Category "ArtCateg" Nothing
    context "auth correct" $ do
      let authInfo = authenticateNormal ("Author", "AuthorPass")
      context "Correct form" $ do
        it "Creates an article in db" $ \port -> do
          (aId, cId) <- hRunDB (A.hDBHandler h) $ do
            aId <- P.insert author
            pId <- P.insert articleCategParent
            cId <- P.insert $ articleCateg {categoryParent = Just pId}
            return (aId, cId)
          let incomingArticle =
                IncomingArticle
                  { incomingTitle = "New Title",
                    incomingCategoryId = cId,
                    incomingContent = "Article content",
                    incomingIsPublished = Just False
                  }
              multipartArticle = multipartIncomingArticle incomingArticle []
          b <- genBoundary
          returnedFormat <- runClientM (create authInfo (b, multipartArticle)) (clientEnv port) >>= shouldBeRightOr "Client or server error"
          Article {..} <- hRunDB (A.hDBHandler h) (P.get (formatArticleId returnedFormat)) >>= shouldBeJustOr "Article was no created but id was returned"
          articleTitle `shouldBe` incomingTitle incomingArticle
          articleContent `shouldBe` incomingContent incomingArticle
          articleCategoryId `shouldBe` cId
          articleUserId `shouldBe` aId
          articleIsPublished `shouldBe` fromMaybe False (incomingIsPublished incomingArticle)

        it "saves all article images" $ \port -> do
          (aId, cId) <- hRunDB (A.hDBHandler h) $ do
            aId <- P.insert author
            pId <- P.insert articleCategParent
            cId <- P.insert $ articleCateg {categoryParent = Just pId}
            return (aId, cId)
          let incomingArticle =
                IncomingArticle
                  { incomingTitle = "New Title",
                    incomingCategoryId = cId,
                    incomingContent = "Article content",
                    incomingIsPublished = Just False
                  }
              fdImages = [image1, image2]
              multipartArticle = multipartIncomingArticle incomingArticle fdImages
          b <- genBoundary
          returnedFormat <- runClientM (create authInfo (b, multipartArticle)) (clientEnv port) >>= shouldBeRightOr "Client or server error"
          let returnedImageIds = formatArticleImages returnedFormat
          createdImages <- hRunDB (A.hDBHandler h) $ do
            artImageIds <- P.selectList [ImageArticleArticleId P.==. formatArticleId returnedFormat] []
            P.selectList [ImageId P.<-. map (imageArticleImageId . P.entityVal) artImageIds] []
          map P.entityKey createdImages `shouldBe` formatArticleImages returnedFormat
          length createdImages `shouldBe` length fdImages
          zipWithM_ (compareImageAndFD h) (map P.entityVal createdImages) fdImages

        it "returns saved article in formated form" $ \port -> do
          (aEnt, pEnt, cEnt) <- hRunDB (A.hDBHandler h) $ do
            aEnt <- P.insertEntity author
            pEnt <- P.insertEntity articleCategParent
            cEnt <- P.insertEntity $ articleCateg {categoryParent = Just (P.entityKey pEnt)}
            return (aEnt, pEnt, cEnt)
          let incomingArticle =
                IncomingArticle
                  { incomingTitle = "New Title",
                    incomingCategoryId = P.entityKey cEnt,
                    incomingContent = "Article content",
                    incomingIsPublished = Just False
                  }
              images = [image1, image2]
              multipartArticle = multipartIncomingArticle incomingArticle images
          b <- genBoundary
          returnedFormat <- runClientM (create authInfo (b, multipartArticle)) (clientEnv port) >>= shouldBeRightOr "Client or server error"
          compareFormatArticleWithIncoming h returnedFormat incomingArticle aEnt (cEnt :| [pEnt]) images

      context "incomplete form" $ do
        it "responds with 400" $ \port -> do
          (aId, cId) <- hRunDB (A.hDBHandler h) $ do
            aId <- P.insert author
            pId <- P.insert articleCategParent
            cId <- P.insert $ articleCateg {categoryParent = Just pId}
            return (aId, cId)
          let multipartArticle = MultipartData [Input "content" "incoming content", Input "category_id" $ T.pack . show . P.unSqlBackendKey . unCategoryKey $ cId] []
          b <- genBoundary
          runClientM (create authInfo (b, multipartArticle)) (clientEnv port) >>= respondsWithErr 400

      context "too many images" $ do
        it "responds with 413" $ \port -> do
          (aEnt, pEnt, cEnt) <- hRunDB (A.hDBHandler h) $ do
            aEnt <- P.insertEntity author
            pEnt <- P.insertEntity articleCategParent
            cEnt <- P.insertEntity $ articleCateg {categoryParent = Just (P.entityKey pEnt)}
            return (aEnt, pEnt, cEnt)
          let incomingArticle =
                IncomingArticle
                  { incomingTitle = "New Title",
                    incomingCategoryId = P.entityKey cEnt,
                    incomingContent = "Article content",
                    incomingIsPublished = Just False
                  }
              images = [image1, image2, image3]
              multipartArticle = multipartIncomingArticle incomingArticle images
          b <- genBoundary
          runClientM (create authInfo (b, multipartArticle)) (clientEnv port) >>= respondsWithErr 413

      context "has too large image" $ do
        it "responds with 413" $ \port -> do
          (aEnt, pEnt, cEnt) <- hRunDB (A.hDBHandler h) $ do
            aEnt <- P.insertEntity author
            pEnt <- P.insertEntity articleCategParent
            cEnt <- P.insertEntity $ articleCateg {categoryParent = Just (P.entityKey pEnt)}
            return (aEnt, pEnt, cEnt)
          let incomingArticle =
                IncomingArticle
                  { incomingTitle = "New Title",
                    incomingCategoryId = P.entityKey cEnt,
                    incomingContent = "Article content",
                    incomingIsPublished = Just False
                  }
              images = [image1, tooLargeImage]
              multipartArticle = multipartIncomingArticle incomingArticle images
          b <- genBoundary
          runClientM (create authInfo (b, multipartArticle)) (clientEnv port) >>= respondsWithErr 413

      context "has not an image" $ do
        it "responds with 415" $ \port -> do
          (aEnt, pEnt, cEnt) <- hRunDB (A.hDBHandler h) $ do
            aEnt <- P.insertEntity author
            pEnt <- P.insertEntity articleCategParent
            cEnt <- P.insertEntity $ articleCateg {categoryParent = Just (P.entityKey pEnt)}
            return (aEnt, pEnt, cEnt)
          let incomingArticle =
                IncomingArticle
                  { incomingTitle = "New Title",
                    incomingCategoryId = P.entityKey cEnt,
                    incomingContent = "Article content",
                    incomingIsPublished = Just False
                  }
              images = [image1, notAnImage]
              multipartArticle = multipartIncomingArticle incomingArticle images
          b <- genBoundary
          runClientM (create authInfo (b, multipartArticle)) (clientEnv port) >>= respondsWithErr 415

    context "incorrect auth" $ do
      context "not an author" $ do
        let authInfo = authenticateNormal ("NotAnAuthor", "NotAnAuthorPass")
        it "responds with 403" $ \port -> do
          (aEnt, pEnt, cEnt) <- hRunDB (A.hDBHandler h) $ do
            aEnt <- P.insertEntity notAnAuthor
            pEnt <- P.insertEntity articleCategParent
            cEnt <- P.insertEntity $ articleCateg {categoryParent = Just (P.entityKey pEnt)}
            return (aEnt, pEnt, cEnt)
          let incomingArticle =
                IncomingArticle
                  { incomingTitle = "New Title",
                    incomingCategoryId = P.entityKey cEnt,
                    incomingContent = "Article content",
                    incomingIsPublished = Just False
                  }
              images = [image1, image2]
              multipartArticle = multipartIncomingArticle incomingArticle images
          b <- genBoundary
          runClientM (create authInfo (b, multipartArticle)) (clientEnv port) >>= respondsWithErr 403

      context "unauthed" $ do
        let authInfo = authenticateNormal ("NotAnAuthor", "NotAnAuthorPass")
        it "responds with 401" $ \port -> do
          (pEnt, cEnt) <- hRunDB (A.hDBHandler h) $ do
            pEnt <- P.insertEntity articleCategParent
            cEnt <- P.insertEntity $ articleCateg {categoryParent = Just (P.entityKey pEnt)}
            return (pEnt, cEnt)
          let incomingArticle =
                IncomingArticle
                  { incomingTitle = "New Title",
                    incomingCategoryId = P.entityKey cEnt,
                    incomingContent = "Article content",
                    incomingIsPublished = Just False
                  }
              images = [image1, image2]
              multipartArticle = multipartIncomingArticle incomingArticle images
          b <- genBoundary
          runClientM (create authInfo (b, multipartArticle)) (clientEnv port) >>= respondsWithErr 401

articleAlterAdd :: A.Handler I.ImageTestIO -> (Port -> ClientEnv) -> SpecWith Port
articleAlterAdd h clientEnv =
  describe "POST article/alter/" $ do
    context "correct auth" $ do
      let authInfo = authenticateNormal ("Author", "AuthorPass")
      context "existing article" $ do
        context "correct form" $ do
          it "saves updates and returns updated article" $ \port -> do
            author <- createTestUser "Author" "AuthorPass" Nothing Nothing False True
            artId <- putTestArticleDef h author
            let updatesArticle = MultipartData [Input "title" "new title", Input "content" "new content"] []
            b <- genBoundary
            formatArticle <-
              runClientM (alterAdd authInfo artId (b, updatesArticle)) (clientEnv port)
                >>= shouldBeRightOr "Internal client server error"
            compareFormatArticleWithDBArticle h formatArticle artId

          it "saves images provided with a form" $ \port -> do
            author <- createTestUser "Author" "AuthorPass" Nothing Nothing False True
            artId <- putTestArticleDef h author
            let image1 = FileData "someImName" "" "image/jpeg" "imContent1" :: FileData Mem
                image2 = FileData "veryRandomName" "" "image/png" "imContent2" :: FileData Mem
                updatesArticle = MultipartData [Input "title" "new title", Input "content" "new content"] [image1, image2]
            b <- genBoundary
            formatArticle <-
              runClientM (alterAdd authInfo artId (b, updatesArticle)) (clientEnv port)
                >>= shouldBeRightOr "Internal client server error"
            compareFormatArticleWithDBArticle h formatArticle artId

        context "incorrect form" $
          it "responds with 400" $ \port -> do
            author <- createTestUser "Author" "AuthorPass" Nothing Nothing False True
            artId <- putTestArticleDef h author
            let updatesArticle = MultipartData [] []
            b <- genBoundary
            runClientM (alterAdd authInfo artId (b, updatesArticle)) (clientEnv port)
              >>= respondsWithErr 400

      context "article does not exist" $ do
        it "responds with 400" $ \port -> do
          author <- createTestUser "Author" "AuthorPass" Nothing Nothing False True
          void $ putTestArticleDef h author
          let updatesArticle = MultipartData [Input "title" "new title", Input "content" "new content"] []
          b <- genBoundary
          void $
            runClientM (alterAdd authInfo (ArticleKey . P.SqlBackendKey $ 2) (b, updatesArticle)) (clientEnv port)
              >>= respondsWithErr 400

    context "incorrect auth" $ do
      context "article doesn't belongs to a user" $ do
        let authInfo = authenticateNormal ("NotAnAuthor", "NotAnAuthorPass")
        it "responds with 403" $ \port -> do
          author <- createTestUser "Author" "AuthorPass" Nothing Nothing False True
          notauthor <- createTestUser "NotAnAuthor" "NotAnAuthorPass" Nothing Nothing False True
          void $ hRunDB (A.hDBHandler h) $ P.insert notauthor
          artId <- putTestArticleDef h author
          let updatesArticle = MultipartData [Input "title" "new title", Input "content" "new content"] []
          b <- genBoundary
          runClientM (alterAdd authInfo artId (b, updatesArticle)) (clientEnv port)
            >>= respondsWithErr 403

articleAlterDelete :: A.Handler I.ImageTestIO -> (Port -> ClientEnv) -> SpecWith Port
articleAlterDelete h clientEnv =
  describe "DELETE article/alter/" $ do
    context "correct auth" $ do
      let authInfo = authenticateNormal ("Author", "AuthorPass")
      context "existing article" $ do
        it "deletes images form an article if they are a part of it" $ \port -> do
          author <- createTestUser "Author" "AuthorPass" Nothing Nothing False True
          artId <- putTestArticleDef h author
          artImages <- hRunDB (A.hDBHandler h) $ do
            imageIds <- P.selectList [ImageArticleArticleId P.==. artId] []
            P.selectList [ImageId P.<-. fmap (imageArticleImageId . P.entityVal) imageIds] []
          DeleteStatus True deleted <-
            runClientM (alterDelete authInfo artId (take 2 (map P.entityKey artImages))) (clientEnv port)
              >>= shouldBeRightOr "Internal client server error"
          deleted `shouldBe` take 2 (map P.entityKey artImages)
          resArtImages <- hRunDB (A.hDBHandler h) $ do
            imageIds <- P.selectList [ImageArticleArticleId P.==. artId] []
            P.selectList [ImageId P.<-. fmap (imageArticleImageId . P.entityVal) imageIds] []
          map P.entityKey resArtImages `shouldBe` drop 2 (map P.entityKey artImages)

        it "doesn't delete images if they are not from the article" $ \port -> do
          author <- createTestUser "Author" "AuthorPass" Nothing Nothing False True
          artId <- putTestArticleDef h author
          artImages <- hRunDB (A.hDBHandler h) $ do
            imageIds <- P.selectList [ImageArticleArticleId P.==. artId] []
            P.selectList [ImageId P.<-. fmap (imageArticleImageId . P.entityVal) imageIds] []
          let externalImage = FileData "someImName" "" "image/jpeg" "imContent1" :: FileData Mem
          (externalImId :| []) <- I.hRunImage (A.hImageHandler h) $ runReaderT (I.saveImages (const $ return ()) (externalImage :| [])) (A.hImageHandler h)
          DeleteStatus True deleted <-
            runClientM (alterDelete authInfo artId $ take 2 (map P.entityKey artImages) <> [externalImId]) (clientEnv port)
              >>= shouldBeRightOr "Internal client server error"
          deleted `shouldBe` take 2 (map P.entityKey artImages)
          resArtImages <- hRunDB (A.hDBHandler h) $ do
            imageIds <- P.selectList [ImageArticleArticleId P.==. artId] []
            P.selectList [ImageId P.<-. fmap (imageArticleImageId . P.entityVal) imageIds] []
          map P.entityKey resArtImages `shouldBe` drop 2 (map P.entityKey artImages)

    context "incorrect auth" $ do
      context "article doesn't belong to a user" $ do
        let authInfo = authenticateNormal ("NotAnAuthor", "NotAnAuthorPass")
        it "responds with 403" $ \port -> do
          author <- createTestUser "Author" "AuthorPass" Nothing Nothing False True
          notauthor <- createTestUser "NotAnAuthor" "NotAnAuthorPass" Nothing Nothing False True
          void $ hRunDB (A.hDBHandler h) $ P.insert notauthor
          artId <- putTestArticleDef h author
          artImages <- hRunDB (A.hDBHandler h) $ do
            imageIds <- P.selectList [ImageArticleArticleId P.==. artId] []
            P.selectList [ImageId P.<-. fmap (imageArticleImageId . P.entityVal) imageIds] []
          runClientM (alterDelete authInfo artId $ take 2 (map P.entityKey artImages)) (clientEnv port)
            >>= respondsWithErr 403

articleGetA :: A.Handler I.ImageTestIO -> (Port -> ClientEnv) -> SpecWith Port
articleGetA h clientEnv = describe "GET article/get/" $ do
  testUser <- runIO $ createTestUser "Author" "AuthorPass" Nothing Nothing False True
  let testCateg pId = Category "TestCateg" (Just pId)
  let filterCateg = Category "FilterCateg" Nothing
  let searchCateg = Category "searchCateg" Nothing
  context "no filters" $ do
    it "responds with the list of articles which length is equal to def pagination limit in asc order" $ \port -> do
      pId <- hRunDB (A.hDBHandler h) $ P.insert $ Category "Parent" Nothing
      filterCategId <- hRunDB (A.hDBHandler h) $ P.insert filterCateg
      articles <- fillDBWithTestArticles h testUser (testCateg pId) filterCategId searchCateg
      resp <-
        runClientM (getA Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) (clientEnv port)
          >>= shouldBeRightOr "Internal client server error"
      pagLimit <-
        runExceptT (flip runReaderT h . A.unApp $ A.askPaginationLimit)
          >>= shouldBeRightOr "Can't get default pagination limit"
      content resp `shouldBe` take (fromIntegral pagLimit) (sortBy (\fa1 fa2 -> formatArticleId fa1 `compare` formatArticleId fa2) articles)

  context "limit less then max" $ do
    it "responds with the list of articles which length is equal to the provided limit in asc order" $ \port -> do
      pId <- hRunDB (A.hDBHandler h) $ P.insert $ Category "Parent" Nothing
      filterCategId <- hRunDB (A.hDBHandler h) $ P.insert filterCateg
      articles <- fillDBWithTestArticles h testUser (testCateg pId) filterCategId searchCateg
      pagLimit <-
        runExceptT (flip runReaderT h . A.unApp $ A.askPaginationLimit)
          >>= shouldBeRightOr "Can't get default pagination limit"
      let limit_ = pagLimit - 1
      resp <-
        runClientM (getA Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just . Limit $ limit_) Nothing) (clientEnv port)
          >>= shouldBeRightOr "Internal client server error"
      content resp `shouldBe` take (fromIntegral limit_) (sortBy (\fa1 fa2 -> formatArticleId fa1 `compare` formatArticleId fa2) articles)

  context "limit more then max" $ do
    it "responds with the list of articles which length is equal to def pagination limit in asc order" $ \port -> do
      pId <- hRunDB (A.hDBHandler h) $ P.insert $ Category "Parent" Nothing
      filterCategId <- hRunDB (A.hDBHandler h) $ P.insert filterCateg
      articles <- fillDBWithTestArticles h testUser (testCateg pId) filterCategId searchCateg
      pagLimit <-
        runExceptT (flip runReaderT h . A.unApp $ A.askPaginationLimit)
          >>= shouldBeRightOr "Can't get default pagination limit"
      let limit_ = pagLimit + 1
      resp <-
        runClientM (getA Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just . Limit $ limit_) Nothing) (clientEnv port)
          >>= shouldBeRightOr "Internal client server error"
      content resp `shouldBe` take (fromIntegral pagLimit) (sortBy (\fa1 fa2 -> formatArticleId fa1 `compare` formatArticleId fa2) articles)

  context "offset" $ do
    it "responds with the list of articles which length is equal to def pagination limit in asc order shifted by offset" $ \port -> do
      pId <- hRunDB (A.hDBHandler h) $ P.insert $ Category "Parent" Nothing
      filterCategId <- hRunDB (A.hDBHandler h) $ P.insert filterCateg
      articles <- fillDBWithTestArticles h testUser (testCateg pId) filterCategId searchCateg
      pagLimit <-
        runExceptT (flip runReaderT h . A.unApp $ A.askPaginationLimit)
          >>= shouldBeRightOr "Can't get default pagination limit"
      let offset_ = 5
      resp <-
        runClientM (getA Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just . Offset $ 5)) (clientEnv port)
          >>= shouldBeRightOr "Internal client server error"
      content resp
        `shouldBe` (take (fromIntegral pagLimit) . drop offset_ . sortBy (\fa1 fa2 -> formatArticleId fa1 `compare` formatArticleId fa2) $ articles)

  context "created_since" $ do
    it "responds with the list of articles created since provided date" $ \port -> do
      pId <- hRunDB (A.hDBHandler h) $ P.insert $ Category "Parent" Nothing
      filterCategId <- hRunDB (A.hDBHandler h) $ P.insert filterCateg
      articles <- fillDBWithTestArticles h testUser (testCateg pId) filterCategId searchCateg
      pagLimit <-
        runExceptT (flip runReaderT h . A.unApp $ A.askPaginationLimit)
          >>= shouldBeRightOr "Can't get default pagination limit"
      currDay <- utctDay <$> getCurrentTime
      resp <-
        runClientM (getA (Just currDay) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) (clientEnv port)
          >>= shouldBeRightOr "Internal client server error"
      content resp
        `shouldBe` ( take (fromIntegral pagLimit) . sortBy (\fa1 fa2 -> formatArticleId fa1 `compare` formatArticleId fa2)
                       . filter (\FormatArticle {..} -> formatArticleCreated > currDay)
                       $ articles
                   )

  context "created_at" $ do
    it "responds with the list of articles created at provided date" $ \port -> do
      pId <- hRunDB (A.hDBHandler h) $ P.insert $ Category "Parent" Nothing
      filterCategId <- hRunDB (A.hDBHandler h) $ P.insert filterCateg
      articles <- fillDBWithTestArticles h testUser (testCateg pId) filterCategId searchCateg
      pagLimit <-
        runExceptT (flip runReaderT h . A.unApp $ A.askPaginationLimit)
          >>= shouldBeRightOr "Can't get default pagination limit"
      currDay <- utctDay <$> getCurrentTime
      resp <-
        runClientM (getA Nothing Nothing (Just currDay) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) (clientEnv port)
          >>= shouldBeRightOr "Internal client server error"
      content resp
        `shouldBe` ( take (fromIntegral pagLimit) . sortBy (\fa1 fa2 -> formatArticleId fa1 `compare` formatArticleId fa2)
                       . filter (\FormatArticle {..} -> formatArticleCreated == currDay)
                       $ articles
                   )

  context "created_until" $ do
    it "responds with the list of articles created until provided date" $ \port -> do
      pId <- hRunDB (A.hDBHandler h) $ P.insert $ Category "Parent" Nothing
      filterCategId <- hRunDB (A.hDBHandler h) $ P.insert filterCateg
      articles <- fillDBWithTestArticles h testUser (testCateg pId) filterCategId searchCateg
      pagLimit <-
        runExceptT (flip runReaderT h . A.unApp $ A.askPaginationLimit)
          >>= shouldBeRightOr "Can't get default pagination limit"
      currDay <- utctDay <$> getCurrentTime
      resp <-
        runClientM (getA Nothing (Just currDay) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) (clientEnv port)
          >>= shouldBeRightOr "Internal client server error"
      content resp
        `shouldBe` ( take (fromIntegral pagLimit) . sortBy (\fa1 fa2 -> formatArticleId fa1 `compare` formatArticleId fa2)
                       . filter (\FormatArticle {..} -> formatArticleCreated < currDay)
                       $ articles
                   )

  context "author_is" $ do
    it "responds with the list of articles from author with provided name" $ \port -> do
      pId <- hRunDB (A.hDBHandler h) $ P.insert $ Category "Parent" Nothing
      filterCategId <- hRunDB (A.hDBHandler h) $ P.insert filterCateg
      articles <- fillDBWithTestArticles h testUser (testCateg pId) filterCategId searchCateg
      pagLimit <-
        runExceptT (flip runReaderT h . A.unApp $ A.askPaginationLimit)
          >>= shouldBeRightOr "Can't get default pagination limit"
      let authorName = "authorName"
      resp <-
        runClientM (getA Nothing Nothing Nothing (Just authorName) Nothing Nothing Nothing Nothing Nothing Nothing Nothing) (clientEnv port)
          >>= shouldBeRightOr "Internal client server error"
      content resp
        `shouldBe` ( take (fromIntegral pagLimit) . sortBy (\fa1 fa2 -> formatArticleId fa1 `compare` formatArticleId fa2)
                       . filter (\FormatArticle {..} -> formatUserUsername formatArticleUser == authorName)
                       $ articles
                   )

  context "category_is" $ do
    it "responds with the list of articles with provided category_id" $ \port -> do
      pId <- hRunDB (A.hDBHandler h) $ P.insert $ Category "Parent" Nothing
      filterCategId <- hRunDB (A.hDBHandler h) $ P.insert filterCateg
      articles <- fillDBWithTestArticles h testUser (testCateg pId) filterCategId searchCateg
      pagLimit <-
        runExceptT (flip runReaderT h . A.unApp $ A.askPaginationLimit)
          >>= shouldBeRightOr "Can't get default pagination limit"
      resp <-
        runClientM (getA Nothing Nothing Nothing Nothing (Just filterCategId) Nothing Nothing Nothing Nothing Nothing Nothing) (clientEnv port)
          >>= shouldBeRightOr "Internal client server error"
      content resp
        `shouldBe` ( take (fromIntegral pagLimit) . sortBy (\fa1 fa2 -> formatArticleId fa1 `compare` formatArticleId fa2)
                       . filter (\FormatArticle {..} -> getNestId formatArticleCategory == filterCategId)
                       $ articles
                   )

  context "title_has" $ do
    it "responds with the list of articles where title contains provided string" $ \port -> do
      pId <- hRunDB (A.hDBHandler h) $ P.insert $ Category "Parent" Nothing
      filterCategId <- hRunDB (A.hDBHandler h) $ P.insert filterCateg
      articles <- fillDBWithTestArticles h testUser (testCateg pId) filterCategId searchCateg
      pagLimit <-
        runExceptT (flip runReaderT h . A.unApp $ A.askPaginationLimit)
          >>= shouldBeRightOr "Can't get default pagination limit"
      let titleHas = "titleHas"
      resp <-
        runClientM (getA Nothing Nothing Nothing Nothing Nothing (Just titleHas) Nothing Nothing Nothing Nothing Nothing) (clientEnv port)
          >>= shouldBeRightOr "Internal client server error"
      content resp
        `shouldBe` ( take (fromIntegral pagLimit) . sortBy (\fa1 fa2 -> formatArticleId fa1 `compare` formatArticleId fa2)
                       . filter (\FormatArticle {..} -> any (isPrefixOf titleHas) (tails formatArticleTitle))
                       $ articles
                   )

  context "content_has" $ do
    it "responds with the list of articles where content contains provided string" $ \port -> do
      pId <- hRunDB (A.hDBHandler h) $ P.insert $ Category "Parent" Nothing
      filterCategId <- hRunDB (A.hDBHandler h) $ P.insert filterCateg
      articles <- fillDBWithTestArticles h testUser (testCateg pId) filterCategId searchCateg
      pagLimit <-
        runExceptT (flip runReaderT h . A.unApp $ A.askPaginationLimit)
          >>= shouldBeRightOr "Can't get default pagination limit"
      let contentHas = "contentHas"
      resp <-
        runClientM (getA Nothing Nothing Nothing Nothing Nothing Nothing (Just contentHas) Nothing Nothing Nothing Nothing) (clientEnv port)
          >>= shouldBeRightOr "Internal client server error"
      content resp
        `shouldBe` ( take (fromIntegral pagLimit) . sortBy (\fa1 fa2 -> formatArticleId fa1 `compare` formatArticleId fa2)
                       . filter (\FormatArticle {..} -> any (isPrefixOf contentHas) (tails formatArticleContent))
                       $ articles
                   )

  context "search" $ do
    it "responds with the list of articles where search of the string was a success" $ \port -> do
      pId <- hRunDB (A.hDBHandler h) $ P.insert $ Category "Parent" Nothing
      filterCategId <- hRunDB (A.hDBHandler h) $ P.insert filterCateg
      articles <- fillDBWithTestArticles h testUser (testCateg pId) filterCategId searchCateg
      pagLimit <-
        runExceptT (flip runReaderT h . A.unApp $ A.askPaginationLimit)
          >>= shouldBeRightOr "Can't get default pagination limit"
      let search = "search"
      resp <-
        runClientM (getA Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just "search") Nothing Nothing Nothing) (clientEnv port)
          >>= shouldBeRightOr "Internal client server error"
      content resp
        `shouldBe` ( take (fromIntegral pagLimit) . sortBy (\fa1 fa2 -> formatArticleId fa1 `compare` formatArticleId fa2)
                       . filter
                         ( \FormatArticle {..} ->
                             any (isPrefixOf search) (tails formatArticleContent)
                               || any (isPrefixOf search) (tails $ getNestName formatArticleCategory)
                               || any (isPrefixOf search) (tails (formatUserUsername formatArticleUser))
                         )
                       $ articles
                   )

  context "sort_by" $ do
    context "date" $
      it "responds with the list of articles sorted by date" $ \port -> do
        pId <- hRunDB (A.hDBHandler h) $ P.insert $ Category "Parent" Nothing
        filterCategId <- hRunDB (A.hDBHandler h) $ P.insert filterCateg
        articles <- fillDBWithTestArticles h testUser (testCateg pId) filterCategId searchCateg
        pagLimit <-
          runExceptT (flip runReaderT h . A.unApp $ A.askPaginationLimit)
            >>= shouldBeRightOr "Can't get default pagination limit"
        resp <-
          runClientM (getA Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just Date) Nothing Nothing) (clientEnv port)
            >>= shouldBeRightOr "Internal client server error"
        content resp
          `shouldBe` ( take (fromIntegral pagLimit) . sortBy (\fa1 fa2 -> formatArticleCreated fa1 `compare` formatArticleCreated fa2) $
                         articles
                     )

    context "author_name" $
      it "responds with the list of articles sorted by author name" $ \port -> do
        pId <- hRunDB (A.hDBHandler h) $ P.insert $ Category "Parent" Nothing
        filterCategId <- hRunDB (A.hDBHandler h) $ P.insert filterCateg
        articles <- fillDBWithTestArticles h testUser (testCateg pId) filterCategId searchCateg
        pagLimit <-
          runExceptT (flip runReaderT h . A.unApp $ A.askPaginationLimit)
            >>= shouldBeRightOr "Can't get default pagination limit"
        resp <-
          runClientM (getA Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just Author) Nothing Nothing) (clientEnv port)
            >>= shouldBeRightOr "Internal client server error"
        content resp
          `shouldBe` ( take (fromIntegral pagLimit)
                         . sortBy (\fa1 fa2 -> (formatUserUsername . formatArticleUser $ fa1) `compare` (formatUserUsername . formatArticleUser $ fa2))
                         $ articles
                     )

    context "category_name" $
      it "responds with the list of articles sorted by category name" $ \port -> do
        pId <- hRunDB (A.hDBHandler h) $ P.insert $ Category "Parent" Nothing
        filterCategId <- hRunDB (A.hDBHandler h) $ P.insert filterCateg
        articles <- fillDBWithTestArticles h testUser (testCateg pId) filterCategId searchCateg
        pagLimit <-
          runExceptT (flip runReaderT h . A.unApp $ A.askPaginationLimit)
            >>= shouldBeRightOr "Can't get default pagination limit"
        resp <-
          runClientM (getA Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just Category_) Nothing Nothing) (clientEnv port)
            >>= shouldBeRightOr "Internal client server error"
        content resp
          `shouldBe` ( take (fromIntegral pagLimit)
                         . sortBy (\fa1 fa2 -> (getNestName . formatArticleCategory $ fa1) `compare` (getNestName . formatArticleCategory $ fa2))
                         $ articles
                     )

    context "ImageNum" $
      it "responds with the list of articles sorted by ImageNum" $ \port -> do
        pId <- hRunDB (A.hDBHandler h) $ P.insert $ Category "Parent" Nothing
        filterCategId <- hRunDB (A.hDBHandler h) $ P.insert filterCateg
        articles <- fillDBWithTestArticles h testUser (testCateg pId) filterCategId searchCateg
        pagLimit <-
          runExceptT (flip runReaderT h . A.unApp $ A.askPaginationLimit)
            >>= shouldBeRightOr "Can't get default pagination limit"
        resp <-
          runClientM (getA Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just ImageNum) Nothing Nothing) (clientEnv port)
            >>= shouldBeRightOr "Internal client server error"
        content resp
          `shouldBe` ( take (fromIntegral pagLimit)
                         . sortBy (\fa1 fa2 -> (length . formatArticleImages $ fa1) `compare` (length . formatArticleImages $ fa2))
                         $ articles
                     )

    context "multiple filters" $
      it "responds with articles applicable to all filters and sorted according to the provided sort" $ \port -> do
        pId <- hRunDB (A.hDBHandler h) $ P.insert $ Category "Parent" Nothing
        filterCategId <- hRunDB (A.hDBHandler h) $ P.insert filterCateg
        articles <- fillDBWithTestArticles h testUser (testCateg pId) filterCategId searchCateg
        let titleHas = "titleHas"
        pagLimit <-
          runExceptT (flip runReaderT h . A.unApp $ A.askPaginationLimit)
            >>= shouldBeRightOr "Can't get default pagination limit"
        curDay <- utctDay <$> getCurrentTime
        resp <-
          runClientM (getA Nothing (Just curDay) Nothing Nothing Nothing (Just titleHas) Nothing Nothing (Just Author) Nothing (Just . Offset $ 1)) (clientEnv port)
            >>= shouldBeRightOr "Internal client server error"
        content resp
          `shouldBe` ( take (fromIntegral pagLimit)
                         . sortBy (\fa1 fa2 -> (formatUserUsername . formatArticleUser $ fa1) `compare` (formatUserUsername . formatArticleUser $ fa2))
                         . filter (\FormatArticle {..} -> formatArticleCreated < curDay)
                         . filter (\FormatArticle {..} -> any (isPrefixOf titleHas) (tails formatArticleTitle))
                         . drop 1
                         $ articles
                     )

data TestArticle = TestArticle
  { taUser :: UserId,
    taCat :: CategoryId,
    taDate :: Maybe Day,
    taTitle :: String,
    taContent :: String,
    taIsPub :: Bool,
    taImages :: [FileData Mem]
  }

defaultTestArticle :: UserId -> CategoryId -> TestArticle
defaultTestArticle defU defC =
  let image1 = FileData "someImName" "" "image/jpeg" "imContent1" :: FileData Mem
      image2 = FileData "veryRandomName" "" "image/png" "imContent2" :: FileData Mem
      image3 = FileData "thirdImage" "" "image/webm" "imContent3" :: FileData Mem
      images = [image1, image2, image3]
   in TestArticle
        { taUser = defU,
          taCat = defC,
          taDate = Nothing,
          taTitle = "defTitle",
          taContent = "defCont",
          taIsPub = True,
          taImages = images
        }

putTestArticle :: A.Handler I.ImageTestIO -> TestArticle -> IO ArticleId
putTestArticle h TestArticle {..} = do
  imEnv <- liftIO . I.unImageTestIO $ I.hPrepareEnv (A.hImageHandler h)
  imFps <- liftIO . I.unImageTestIO $ mapM (I.hPutImage (A.hImageHandler h) imEnv) taImages
  imIds <- hRunDB (A.hDBHandler h) $ zipWithM (\ct fp -> P.insert $ Image ct fp) (map (T.unpack . fdFileCType) taImages) imFps
  date <- maybe (utctDay <$> getCurrentTime) return taDate
  artId <-
    hRunDB (A.hDBHandler h) $
      P.insert $
        Article
          { articleTitle = taTitle,
            articleCreated = date,
            articleUserId = taUser,
            articleCategoryId = taCat,
            articleContent = taContent,
            articleIsPublished = taIsPub
          }
  void $ hRunDB (A.hDBHandler h) $ mapM_ (P.insert . ImageArticle artId) imIds
  return artId

putTestArticleDef :: A.Handler I.ImageTestIO -> User -> IO ArticleId
putTestArticleDef h author = do
  let image1 = FileData "someImName" "" "image/jpeg" "imContent1" :: FileData Mem
      image2 = FileData "veryRandomName" "" "image/png" "imContent2" :: FileData Mem
      image3 = FileData "thirdImage" "" "image/webm" "imContent3" :: FileData Mem
      images = [image1, image2, image3]
  let articleCategParent = Category "Parent art categ" Nothing
      articleCateg = Category "ArtCateg" Nothing
  pId <- hRunDB (A.hDBHandler h) $ P.insert articleCategParent
  aId <- hRunDB (A.hDBHandler h) $ P.insert author
  cId <- hRunDB (A.hDBHandler h) $ P.insert articleCateg {categoryParent = Just pId}
  putTestArticle h (TestArticle aId cId Nothing "some title" "some content" False images)

fillDBWithTestArticles :: A.Handler I.ImageTestIO -> User -> Category -> CategoryId -> Category -> IO [FormatArticle]
fillDBWithTestArticles h defU defC filterCategId searchCateg = do
  defUId <- hRunDB (A.hDBHandler h) $ P.insert defU
  defCategId <- hRunDB (A.hDBHandler h) $ P.insert defC
  curTime <- utctDay <$> getCurrentTime
  lastDayArtId <- putTestArticle h (defaultTestArticle defUId defCategId) {taTitle = "lastDay", taDate = Just (ModifiedJulianDay $ toModifiedJulianDay curTime - 1)}
  curDayArtId <- putTestArticle h (defaultTestArticle defUId defCategId) {taTitle = "curDay"}
  nextDayArtId <- putTestArticle h (defaultTestArticle defUId defCategId) {taTitle = "nextDay", taDate = Just (ModifiedJulianDay $ toModifiedJulianDay curTime + 1)}
  authorNameUId <- hRunDB (A.hDBHandler h) $ P.insert $ defU {userName = "authorName"}
  authorNameArtId <- putTestArticle h (defaultTestArticle authorNameUId defCategId)
  catIdArtId <- putTestArticle h (defaultTestArticle defUId filterCategId)
  titleHasArtId <- putTestArticle h (defaultTestArticle defUId defCategId) {taTitle = "1titleHas1"}
  contentHasArtId <- putTestArticle h (defaultTestArticle defUId defCategId) {taContent = "1contentHas1"}
  searchAuthorUId <- hRunDB (A.hDBHandler h) $ P.insert $ defU {userName = "1search1"}
  searchAuthorArtId <- putTestArticle h (defaultTestArticle searchAuthorUId defCategId)
  searchContentArtId <- putTestArticle h (defaultTestArticle defUId defCategId) {taContent = "1searchContent1"}
  searchCategId <- hRunDB (A.hDBHandler h) $ P.insert searchCateg
  searchCategArtId <- putTestArticle h (defaultTestArticle defUId searchCategId)
  hRunDB (A.hDBHandler h) $
    mapM
      (fmap fromJust . getFormatArticle)
      [ lastDayArtId,
        curDayArtId,
        nextDayArtId,
        authorNameArtId,
        catIdArtId,
        titleHasArtId,
        contentHasArtId,
        searchAuthorArtId,
        searchCategArtId,
        searchContentArtId
      ]

compareFormatArticleWithDBArticle :: A.Handler I.ImageTestIO -> FormatArticle -> ArticleId -> Expectation
compareFormatArticleWithDBArticle h format1 artId = do
  format2 <- hRunDB (A.hDBHandler h) (getFormatArticle artId) >>= shouldBeJustOr "DB error"
  format1 `shouldBe` format2

compareFormatArticleWithIncoming :: A.Handler I.ImageTestIO -> FormatArticle -> IncomingArticle -> P.Entity User -> NonEmpty (P.Entity Category) -> [FileData Mem] -> Expectation
compareFormatArticleWithIncoming h FormatArticle {..} incArt uId catTree fds = do
  formatArticleTitle `shouldBe` incomingTitle incArt
  formatArticleContent `shouldBe` incomingContent incArt
  formatArticleIsPublished `shouldBe` fromMaybe False (incomingIsPublished incArt)
  let nestedCats = parseListToNest catTree
      formatUser = formatEntityUser uId
  createdImages <- hRunDB (A.hDBHandler h) $ do
    artImageIds <- P.selectList [ImageArticleArticleId P.==. formatArticleId] []
    P.selectList [ImageId P.<-. map (imageArticleImageId . P.entityVal) artImageIds] []
  map P.entityKey createdImages `shouldBe` formatArticleImages
  formatArticleCategory `shouldBe` nestedCats
  formatArticleUser `shouldBe` formatUser

compareImageAndFD :: A.Handler I.ImageTestIO -> Image -> FileData Mem -> Expectation
compareImageAndFD h im fd = do
  imageData <- liftIO . I.unImageTestIO $ I.hGetImage (A.hImageHandler h) (imagePath im)
  imageData `shouldBe` LBS.toStrict (fdPayload fd)
  imageCt im `shouldBe` T.unpack (fdFileCType fd)