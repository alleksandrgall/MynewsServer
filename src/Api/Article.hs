{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Api.Article where

import Api.Article.Filters
  ( GetWithFilters,
    SortBy,
    authorNameF,
    categoryIdF_,
    contentHasF,
    createdAtF,
    createdSinceF,
    createdUntilF,
    searchF,
    sortByF_,
    titleHasF,
  )
import Api.Article.Get
  ( FormatArticle,
    getFormatArticle,
    getFormatArticlesPagination,
  )
import Api.Internal.Auth (articleBelongsToUser, userAtLeastAuthor_)
import Api.Internal.Optional (MaybeSetter (..), setsMaybe)
import Api.Internal.Pagination (Limit, Offset, WithOffset)
import Control.Monad (unless, void, when)
import Control.Monad.Catch (MonadCatch, MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (asks)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text as T
import Data.Time (Day, UTCTime (utctDay), getCurrentTime)
import Database.Esqueleto.Experimental
  ( BackendKey (SqlBackendKey),
    Value (unValue),
    from,
    innerJoin,
    on,
    select,
    table,
    val,
    where_,
    (&&.),
    (==.),
    (^.),
    type (:&) ((:&)),
  )
import qualified Database.Persist as P
import GHC.Generics (Generic)
import Handlers.App (App, Auth (Auth), Handler (hImageHandler), askPaginationLimit, runDB)
import Handlers.App.HandleExcept (handleDeleteException, handleFormatExcept, handlePutException)
import Handlers.DB.Scheme
  ( Article (Article),
    ArticleId,
    CategoryId,
    EntityField (ImageArticleArticleId, ImageArticleImageId, ImageId),
    Image,
    ImageArticle (ImageArticle),
    ImageId,
    Key (CategoryKey),
    UserId,
  )
import Handlers.Image
import qualified Handlers.Image as I
import qualified Katip as K
import Servant
  ( AuthProtect,
    Capture,
    Delete,
    HasServer (ServerT),
    JSON,
    Post,
    Proxy (..),
    PutCreated,
    QueryParams,
    ServerError (errReasonPhrase),
    err400,
    err500,
    throwError,
    type (:<|>) (..),
    type (:>),
  )
import Servant.Multipart
  ( FromMultipart (..),
    Mem,
    MultipartData (files),
    MultipartForm,
    lookupInput,
  )
import qualified Text.Read as T

type ArticleApi =
  AuthProtect "normal" :> "create" :> MultipartForm Mem (MultipartData Mem) :> PutCreated '[JSON] FormatArticle -- json article and photoes in form
    :<|> AuthProtect "normal" :> "alter" :> Capture "article_id" ArticleId :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] FormatArticle -- alter article by sending new IncomingArticle and/or images
    :<|> AuthProtect "normal" :> "alter" :> Capture "article_id" ArticleId :> QueryParams "image_id" ImageId :> Delete '[JSON] DeleteStatus -- returns number of deleted images
    :<|> ("get" :> GetWithFilters '[JSON] FormatArticle)

articleApi :: Proxy ArticleApi
articleApi = Proxy

articleServer :: (MonadMask imageM, MonadIO imageM) => ServerT ArticleApi (App imageM)
articleServer = create :<|> alterAdd :<|> alterDelete :<|> getA

--  Creating article
data IncomingArticle = IncomingArticle
  { incomingTitle :: String,
    incomingCategoryId :: CategoryId,
    incomingContent :: String,
    incomingIsPublished :: Maybe Bool
  }
  deriving (Show, Generic)

instance FromMultipart Mem IncomingArticle where
  fromMultipart form = parseMultipart
    where
      parseMultipart =
        IncomingArticle <$> (T.unpack <$> lookupInput "title" form)
          <*> (lookupInput "category_id" form >>= (fmap (CategoryKey . SqlBackendKey) . T.readEither . T.unpack))
          <*> (T.unpack <$> lookupInput "content" form)
          <*> either (\_ -> Right Nothing) (Right . Just) (lookupInput "is_published" form >>= T.readEither . T.unpack)

incomingArticleToDbArticle :: IncomingArticle -> UserId -> IO Article
incomingArticleToDbArticle IncomingArticle {..} uId = do
  day <- utctDay <$> getCurrentTime
  return $ Article incomingTitle day uId incomingCategoryId incomingContent (fromMaybe False incomingIsPublished)

create :: (MonadMask imageM, MonadIO imageM) => Auth a -> MultipartData Mem -> App imageM FormatArticle
create (Auth u) form = do
  K.logFM K.InfoS "Creating an article"
  userAtLeastAuthor_ u
  case fromMultipart form of
    Left e -> throwError err400 {errReasonPhrase = "Absent or incomplete article content. Error: " ++ e}
    Right incArt -> do
      dbArticle <- liftIO $ incomingArticleToDbArticle incArt (P.entityKey u)
      aId <- runDB $ P.insert dbArticle
      case files form of
        [] -> pure ()
        (fd : fds) -> do
          imH <- asks hImageHandler
          void $
            handlePutException . handleFormatExcept $
              I.runImage imH $ saveImages (void . P.insert . ImageArticle aId) (fd :| fds)
      K.katipAddContext (K.sl "article_id" aId) $
        K.logFM K.InfoS "Article created"
      maybeFormatArt <- runDB $ getFormatArticle aId
      maybe
        ( $(K.logTM) K.EmergencyS "DB is unstable."
            >> throwError err500 {errReasonPhrase = "Critical error"}
        )
        return
        maybeFormatArt

-- Altering article by adding pictures or changing non picture fields
alterAdd :: (MonadMask imageM, MonadIO imageM) => Auth a -> ArticleId -> MultipartData Mem -> App imageM FormatArticle
alterAdd (Auth u) aId form = do
  K.katipAddContext (K.sl "article_id" aId) $ do
    K.logFM K.InfoS "Altering an article"
    articleBelongsToUser u aId
    case fromMultipart form of
      Left _ -> throwError err500 {errReasonPhrase = "Unable to parse setters."}
      Right setters -> do
        let isEmptySetters = all (\(MaybeSetter x) -> isNothing . snd $ x) setters
            isEmptyImages = null (files form)
        when (isEmptyImages && isEmptySetters) $ throwError err400 {errReasonPhrase = "Nothing to be set."}
        case files form of
          [] -> pure ()
          (fd : fds) -> do
            imH <- asks hImageHandler
            void . handlePutException . handleFormatExcept $
              I.runImage imH $ saveImages (void . P.insert . ImageArticle aId) (fd :| fds)
        unless isEmptySetters $ runDB $ P.update aId (setsMaybe setters)
        K.logFM K.InfoS "Article altered"
        maybeFormatArt <- runDB $ getFormatArticle aId
        maybe
          ( $(K.logTM) K.EmergencyS "DB is unstable."
              >> throwError err500 {errReasonPhrase = "Critical error"}
          )
          return
          maybeFormatArt

--Deleting pictures from the article
alterDelete :: (MonadCatch imageM, MonadIO imageM) => Auth a -> ArticleId -> [ImageId] -> App imageM DeleteStatus
alterDelete (Auth u) aId imIds = do
  K.katipAddContext (K.sl "article_id" aId) $ do
    K.logFM K.InfoS "Deleting images form article"
    articleBelongsToUser u aId
    articleImages <- runDB $
      select $ do
        (im :& imArt) <-
          from $
            table @Image
              `innerJoin` table @ImageArticle
              `on` \(im :& imArt) -> im ^. ImageId ==. imArt ^. ImageArticleImageId
        where_ (imArt ^. ImageArticleArticleId ==. val aId)
        return (im ^. ImageId)
    let filteredImIds = filter (`elem` map unValue articleImages) imIds
    imH <- asks hImageHandler
    delStatus <- handleDeleteException $ I.runImage imH $ I.deleteImages filteredImIds
    K.katipAddContext (K.sl "delete_success" (deleteStatus delStatus)) $ K.logFM K.InfoS "Images deleted" >> return delStatus

-- Getting article with filters, search and sort
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
  App imageM (WithOffset FormatArticle)
getA createdSince createdUntil createdAt authorName categoryId_ titleHas contentHas searchStr sortBy_ lim off = do
  K.katipAddContext
    ( K.sl "created_since" createdSince <> K.sl "created_until" createdUntil
        <> K.sl "created_at" createdAt
        <> K.sl "author_name" authorName
        <> K.sl "category_id" categoryId_
        <> K.sl "title_has" titleHas
        <> K.sl "content_has" contentHas
        <> K.sl "search_string" searchStr
        <> K.sl "sort_by" (show sortBy_)
        <> K.sl "limit" lim
        <> K.sl "offset" off
    )
    $ do
      K.logFM K.InfoS "Sending articles"
      maxLim <- askPaginationLimit
      articles <-
        runDB $
          getFormatArticlesPagination
            ( \art user_ cat ->
                createdSinceF art createdSince
                  &&. createdUntilF art createdUntil
                  &&. createdAtF art createdAt
                  &&. authorNameF user_ authorName
                  &&. categoryIdF_ cat categoryId_
                  &&. titleHasF art titleHas
                  &&. contentHasF art contentHas
                  &&. searchF art user_ cat searchStr
            )
            ( \art user_ cat imageNum ->
                sortByF_ art user_ cat imageNum sortBy_
            )
            lim
            maxLim
            off
      K.katipAddContext (K.sl "article_num" (length articles)) $ K.logFM K.InfoS "Articles sent" >> return articles
