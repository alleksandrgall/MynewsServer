{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Api.Internal.ImageManager
  ( DeleteStatus (deleteStatus),
    deleteImagesArticle,
    saveAndInsertImages,
  )
import Api.Internal.Optional (MaybeSetter (..), setsMaybe)
import Api.Internal.Pagination (Limit, Offset, WithOffset)
import Control.Applicative ((<|>))
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (isNothing)
import Data.String (IsString (fromString))
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (Day, UTCTime (utctDay), getCurrentTime)
import Database.Esqueleto.Experimental
  ( BackendKey (SqlBackendKey),
    (&&.),
  )
import qualified Database.Persist as P
import GHC.Generics (Generic)
import Handlers.App (App, Auth (Auth), askPaginationLimit, runDB)
import Handlers.DB.Scheme
  ( Article (Article),
    ArticleId,
    Category,
    CategoryId,
    ImageArticle (ImageArticle),
    ImageId,
    Key (CategoryKey),
    UserId,
  )
import qualified Katip as K
import Servant
  ( AuthProtect,
    Capture,
    DeleteAccepted,
    HasServer (ServerT),
    JSON,
    PostAccepted,
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
  AuthProtect "normal"
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

instance A.FromJSON IncomingArticle where
  parseJSON = A.genericParseJSON A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_' . drop 8}

instance FromMultipart Mem IncomingArticle where
  fromMultipart form = (lookupInput "article" form >>= A.eitherDecode . LBS.fromStrict . encodeUtf8) <|> parseMultipart
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

create :: Auth a -> MultipartData Mem -> App FormatArticle
create (Auth u) form = do
  K.logFM K.InfoS "Creating an article"
  userAtLeastAuthor_ u
  case fromMultipart form of
    Left e -> throwError err400 {errReasonPhrase = "Absent or incomplete article content. Error: " ++ e}
    Right incArt -> do
      let fds = files form
      K.logFM K.InfoS (fromString . show . length $ fds)
      mapM_ (\fd -> K.logFM K.InfoS (fromString . show $ fd)) fds
      imageIds <- saveAndInsertImages fds (\_ -> return ())
      dbArticle <- liftIO $ incomingArticleToDbArticle incArt (P.entityKey u)
      aId <- runDB $ do
        aId <- P.insert dbArticle
        mapM_ (P.insert . ImageArticle aId) imageIds
        return aId
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
alterAdd :: Auth a -> ArticleId -> MultipartData Mem -> App FormatArticle
alterAdd (Auth u) aId form = do
  K.katipAddContext (K.sl "article_id" aId) $ do
    K.logFM K.InfoS "Altering an article"
    articleBelongsToUser u aId
    case fromMultipart form of
      Left _ -> throwError err500 {errReasonPhrase = "Unable to parse setters."}
      Right setters -> do
        let isEmptySetters = all (\(MaybeSetter x) -> isNothing . snd $ x) setters
            isEmptyImages = null (files form)
        when (null (files form) && isEmptySetters) $ throwError err400 {errReasonPhrase = "Nothing to be set."}
        unless isEmptyImages $ void $ saveAndInsertImages (files form) (void . P.insert . ImageArticle aId)
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

alterDelete :: Auth a -> ArticleId -> [ImageId] -> App DeleteStatus
alterDelete (Auth u) aId imIds = do
  K.katipAddContext (K.sl "article_id" aId) $ do
    K.logFM K.InfoS "Deleting images form article"
    articleBelongsToUser u aId
    delStatus <- deleteImagesArticle imIds aId
    K.katipAddContext (K.sl "delete_success" (deleteStatus delStatus)) $ K.logFM K.InfoS "Images deleted" >> return delStatus

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
  App (WithOffset FormatArticle)
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
