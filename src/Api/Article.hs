{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Api.Article where

import Api.Article.Filters
import Api.Article.Get
import Api.Internal.Auth (articleBelongsToUser, userAtLeastAuthor_)
import Api.Internal.ImageManager
import Api.Internal.Optional
import Api.Internal.Pagination (GetWithPagination, Limit, Offset, WithOffset)
import App
import Control.Applicative ((<|>))
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (liftIO)
import DB.Scheme
import Data.Aeson hiding (Value)
import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import qualified Data.Map as M
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import Database.Esqueleto.Experimental hiding (isNothing)
import qualified Database.Persist as P
import Database.Persist.Sql (SqlPersistM)
import GHC.Generics (Generic)
import Katip (Severity (InfoS), katipAddContext, katipAddNamespace, logFM, logTM, sl)
import Servant
import Servant.Multipart
import qualified Text.Read as T

type ArticleApi =
  BasicAuth "Is author" (P.Entity User)
    :> ( "create" :> MultipartForm Mem (MultipartData Mem) :> PutCreated '[JSON] FormatArticle -- json article and photoes in form
           :<|> "alter" :> Capture "article_id" ArticleId
             :> ( MultipartForm Mem (MultipartData Mem) :> PostAccepted '[JSON] FormatArticle -- alter article by sending new IncomingArticle and/or images
                    :<|> QueryParams "image_id" ImageId :> DeleteAccepted '[JSON] DeleteStatus -- returns number of deleted images
                )
       )
    :<|> ("get" :> GetWithFilters '[JSON] FormatArticle)

articleApi :: Proxy ArticleApi
articleApi = Proxy

articleServer :: ServerT ArticleApi App
articleServer = authorApi :<|> getA
  where
    authorApi user = create user :<|> alter user
    alter user articleId = alterAdd user articleId :<|> alterDelete user articleId

--  Creating article
data IncomingArticle = IncomingArticle
  { incomingTitle :: String,
    incomingCategoryId :: CategoryId,
    incomingContent :: String,
    incomingIsPublished :: Bool
  }
  deriving (Show, Generic)

instance FromJSON IncomingArticle where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 8}

instance FromMultipart Mem IncomingArticle where
  fromMultipart form = (lookupInput "article" form >>= eitherDecode . LBS.fromStrict . encodeUtf8) <|> parseMultipart
    where
      parseMultipart =
        IncomingArticle <$> (T.unpack <$> lookupInput "title" form)
          <*> (lookupInput "category_id" form >>= (fmap (CategoryKey . SqlBackendKey) . T.readEither . T.unpack))
          <*> (T.unpack <$> lookupInput "content" form)
          <*> withDef (lookupInput "is_published" form >>= T.readEither . T.unpack) False
      withDef (Right x) _ = Right x
      withDef _ def = Right def

incomingArticleToDbArticle :: IncomingArticle -> UserId -> IO Article
incomingArticleToDbArticle IncomingArticle {..} uId = do
  day <- utctDay <$> getCurrentTime
  return $ Article incomingTitle day uId incomingCategoryId incomingContent incomingIsPublished

create :: P.Entity User -> MultipartData Mem -> App FormatArticle
create u form = do
  logFM InfoS "Creating an article"
  userAtLeastAuthor_ u
  case fromMultipart form of
    Left e -> throwError err400 {errReasonPhrase = "Absent or incomplete article content. Error: " ++ e}
    Right incArt -> do
      let fds = files form
      imageIds <- saveAndInsertImages fds (\_ -> return ())
      dbArticle <- liftIO $ incomingArticleToDbArticle incArt (P.entityKey u)
      aId <- runDB $ do
        aId <- P.insert dbArticle
        mapM_ (P.insert . ImageArticle aId) imageIds
        return aId
      katipAddContext (sl "article_id" aId) $
        logFM InfoS "Created"
      runDB $ getFormatArticle aId

-- Altering article by adding pictures or changing non picture fields
instance FromMultipart Mem [MaybeSetter Article] where
  fromMultipart form =
    Right
      [ MaybeSetter (ArticleTitle, lookupF (Just . T.unpack) "title"),
        MaybeSetter (ArticleCategoryId, lookupF (fmap (CategoryKey . SqlBackendKey) . T.readMaybe . T.unpack) "category_id"),
        MaybeSetter (ArticleContent, lookupF (Just . T.unpack) "content"),
        MaybeSetter (ArticleIsPublished, lookupF (T.readMaybe . T.unpack) "is_published")
      ]
    where
      lookupF f name = either (const Nothing) f (lookupInput name form)

alterAdd :: P.Entity User -> ArticleId -> MultipartData Mem -> App FormatArticle
alterAdd u aId form = do
  katipAddContext (sl "article_id" aId) $ do
    logFM InfoS "Altering an article"
    articleBelongsToUser u aId
    case fromMultipart form of
      Left e -> throwError err500 {errReasonPhrase = "Unable to parse setters."}
      Right setters -> do
        let isEmptySetters = all (\(MaybeSetter x) -> isNothing . snd $ x) setters
            isEmptyImages = null (files form)
        when (null (files form) && isEmptySetters) $ throwError err400 {errReasonPhrase = "Nothing to be set."}
        unless isEmptyImages $ void $ saveAndInsertImages (files form) (void . P.insert . ImageArticle aId)
        unless isEmptySetters $ runDB $ P.update aId (setsMaybe setters)
        logFM InfoS "Article altered"
        runDB $ getFormatArticle aId

--Deleting pictures from the article

alterDelete :: P.Entity User -> ArticleId -> [ImageId] -> App DeleteStatus
alterDelete u aId imIds = do
  katipAddContext (sl "article_id" aId) $ do
    logFM InfoS "Deleting images form article"
    articleBelongsToUser u aId
    deleteImagesArticle imIds aId

-- Getting article with filters, search and sort

getA ::
  Maybe Day -> -- created_since
  Maybe Day -> -- created_until
  Maybe Day -> -- created_at
  Maybe String -> -- author_name
  Maybe (Key Category) -> -- category
  Maybe String -> -- title_has
  Maybe String -> -- content_has
  Maybe String -> -- search
  Maybe SortBy -> -- sortParam
  Maybe Limit ->
  Maybe Offset ->
  AppT IO (WithOffset FormatArticle)
getA createdSince createdUntil createdAt authorName categoryName_ titleHas contentHas searchStr sortBy_ lim off = do
  maxLim <- askPaginationLimit
  runDB $
    getFormatArticlesPagination
      ( \art user_ cat imageNum ->
          maybeFilter createdSince (createdSinceF art)
            &&. maybeFilter createdUntil (createdUntilF art)
            &&. maybeFilter createdAt (createdAtF art)
            &&. maybeFilter authorName (authorNameF user_)
            &&. maybeFilter categoryName_ (categoryNameF_ cat)
            &&. maybeFilter titleHas (titleHasF art)
            &&. maybeFilter contentHas (contentHasF art)
            &&. maybeFilter searchStr (searchF art user_ cat)
      )
      ( \art user_ cat imageNum ->
          maybeSort sortBy_ (sortByF_ art user_ cat imageNum)
      )
      lim
      maxLim
      off
