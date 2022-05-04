{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Api.Pagination
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
    :<|> ("get" :> ArticleFilters :> QueryParam "sort_by" SortBy :> Get '[JSON] [InternalArticle])

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

type ImageApi = Capture "image_id" ImageId :> Raw
