{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Api.Article where

import Api.Auth (userIsAuthor_)
import App
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import DB.Scheme
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import Database.Persist
import Dev (runDBDev)
import GHC.Generics (Generic)
import Servant
import Servant.Multipart
import qualified Text.Read as T
import Utils (saveInsertToDbImages, validateImages)

type ArticleApi =
  BasicAuth "Is author" (Entity User) :> "create" :> MultipartForm Mem (MultipartData Mem) :> PostCreated '[JSON] ArticleId -- json article and photoes in form
    :<|> BasicAuth "Is author" (Entity User) :> "alter"
      :> ( Capture "article_id" ArticleId :> "text" :> MultipartForm Mem IncomingArticle :> PostNoContent
             :<|> Capture "article_id" ArticleId :> "images" :> MultipartForm Mem (MultipartData Mem) :> PostAccepted '[JSON] [ImageId] -- returns all image_ids of new images
             :<|> Capture "article_id" ArticleId :> QueryParams "ids" [ImageId] :> DeleteAccepted '[JSON] Int -- returns number of deleted images
         )
    :<|> "get" :> ArticleFilters :> QueryParam "sort_by" SortBy :> Get '[JSON] [InternalArticle]

articleApi = Proxy :: Proxy ArticleApi

articleServer :: ServerT ArticleApi App
articleServer = create :<|> undefined :<|> undefined

data IncomingArticle = IncomingArticle
  { incomingTitle :: String,
    incomingCategoryId :: CategoryId,
    incomingContent :: String,
    incomingIsPublish :: Bool
  }
  deriving (Show, Generic)

instance FromJSON IncomingArticle where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 8}

instance FromMultipart Mem IncomingArticle where
  fromMultipart form = parseMultipart <|> (lookupInput "article" form >>= eitherDecode . LBS.fromStrict . encodeUtf8)
    where
      parseMultipart =
        IncomingArticle <$> (T.unpack <$> lookupInput "title" form)
          <*> (lookupInput "category_id" form >>= T.readEither . T.unpack)
          <*> (T.unpack <$> lookupInput "content" form)
          <*> withDef (lookupInput "is_published" form >>= T.readEither . T.unpack) False
      withDef (Right x) _ = Right x
      withDef _ def = Right def

create :: Entity User -> MultipartData Mem -> App ArticleId
create u form = do
  userIsAuthor_ u
  case fromMultipart form of
    Left e -> throwError err400 {errReasonPhrase = "Absent or incomplete article content."}
    Right IncomingArticle {..} -> do
      let fds = files form
      vs <- validateImages fds
      imageRoot <- askImageRoot
      imageIds <- liftIO $ saveInsertToDbImages vs imageRoot (\_ -> return ())
      runDBDev $ do
        aId <-
          insert $
            Article
              { articleTitle = incomingTitle,
                articleUserId = entityKey u,
                articleCategoryId = incomingCategoryId,
                articleContent = incomingContent,
                articleIsPublished = incomingIsPublish
              }
        mapM_ (insert . ImageArticle aId) imageIds
        return aId

data InternalArticle = InternalArticle
  { iArticleTitle :: String,
    iArticleUser :: InternalUser,
    iArticleCategory :: NestCategory,
    iArticleContent :: String,
    iArticleImages :: [ImageId]
  }
  deriving (Generic)

data InternalUser = InternalUser
  { iUserName :: String,
    iUserAvatar :: Maybe String,
    iUserCreated :: UTCTime,
    iUserIsAdmin :: Bool,
    iUserIsAuthor :: Bool
  }

data NestCategory = NestCategory CategoryId String NestCategory | Non
  deriving (Show)

instance ToJSON NestCategory where
  toJSON Non = object []
  toJSON (NestCategory i name children) =
    object
      [ "id" .= i,
        "category_name" .= T.pack name,
        "category_sub" .= toJSON children
      ]

parseListToNest :: [Entity Category] -> NestCategory
parseListToNest ents =
  let mEnt = M.fromList $ map (\ent -> (categoryParent . entityVal $ ent, ent)) ents
      root = M.lookup Nothing mEnt
      children Nothing = Non
      children (Just c) =
        NestCategory (entityKey c) (categoryName . entityVal $ c) $ children . M.lookup (Just $ entityKey c) $ mEnt
   in children root

data SortBy = Date | Author | Category_ | ImageNum

instance ToHttpApiData SortBy where
  toUrlPiece Date = "date"
  toUrlPiece Author = "author"
  toUrlPiece Category_ = "category"
  toUrlPiece ImageNum = "image_number"

instance FromHttpApiData SortBy where
  parseUrlPiece t
    | t == "date" = Right Date
    | t == "author" = Right Author
    | t == "category" = Right Category_
    | t == "image_number" = Right ImageNum
    | otherwise = Left "Use one: \"date\", \"author\", \"category\", \"image_number\""

type ArticleFilters =
  QueryParam "created_since" Day :> QueryParam "created_until" Day :> QueryParam "created_at" Day
    :> QueryParam "author" String
    :> QueryParam "category" CategoryId
    :> QueryParam "title_has" String
    :> QueryParam "content_has" String
    -- API новостей должно поддерживать поиск по строке,
    --которая может быть найдена либо в текстовом контенте, либо в имени автора, либо в названии категории.
    :> QueryParam "search" String
