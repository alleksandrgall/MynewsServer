{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import DB.Scheme
import DB.Types.Internal
import Data.Aeson (ToJSON, decode)
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (Day, UTCTime)
import Dev
import GHC.Generics (Generic)
import Servant
import Servant.Multipart

type Pagination = QueryParam "offset" Int :> QueryParam "limit" Int

data WithOffset a = WithOffset {offset :: Int, content :: a}
  deriving (Generic, Show, ToJSON)

type UserApi =
  BasicAuth "admin" User
    :> ( "create" :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] (WithStatus 201 UserId)
           :<|> "to_author" :> QueryParam' '[Required] "username" String :> Post '[JSON] (WithStatus 202 ())
       )
      :<|> ( "get"
               :> Pagination
               :> Get '[JSON] [InternalUser]
           )

instance FromMultipart Mem IncomingUser where
  fromMultipart form = case decode . LBS.fromStrict . encodeUtf8 <$> lookupInput "user" form of
    Left e -> Left e
    Right Nothing -> Left "Bad user data."
    Right (Just u) -> Right u

type ArticleApi =
  BasicAuth "Is author" User
    :> ( "create" :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] (WithStatus 201 ArticleId) -- json article and photoes in form
           :<|> Capture "article_id" ArticleId :> "alter"
             :> ( "text" :> MultipartForm Mem Article :> Post '[JSON] (WithStatus 202 ())
                    :<|> "images"
                      :> ( MultipartForm Mem (MultipartData Mem) :> Post '[JSON] (WithStatus 202 [ImageId]) -- returns all image_ids of new images
                             :<|> QueryParams "ids" [ImageId] :> Delete '[JSON] (WithStatus 202 Int) -- returns number of deleted images
                         )
                )
       )
    :<|> ("get" :> Pagination :> ArticleFilters :> QueryParam "sort_by" SortBy :> Get '[JSON] [InternalArticle])

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

type CategoryApi =
  BasicAuth "admin" User
    :> ( "create" :> QueryParam' [Required, Strict] "name" String :> QueryParam' [Required, Strict] "parent" CategoryId :> Post '[JSON] (WithStatus 201 CategoryId)
           :<|> "alter" :> Capture "category_id" CategoryId
             :> QueryParam' [Required, Strict] "name" String
             :> QueryParam' [Required, Strict] "parent" CategoryId
             :> Post '[JSON] (WithStatus 202 ())
       )
    :<|> ("get" :> Pagination :> Get '[JSON] [Category])

type ImageApi = Capture "image_id" ImageId :> Raw

type Api =
  "users" :> UserApi
    :<|> "news" :> ArticleApi
    :<|> "category" :> CategoryApi
    :<|> "image" :> ImageApi

api :: Proxy Api
api = Proxy
