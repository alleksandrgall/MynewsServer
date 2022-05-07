{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Article where

import DB.Scheme
import Data.Aeson
import qualified Data.Map as M
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Data.Time
import Database.Persist
import Servant
import Servant.Multipart

type ArticleApi =
  BasicAuth "Is author" User :> "create" :> MultipartForm Mem (MultipartData Mem) :> PostCreated '[JSON] ArticleId -- json article and photoes in form
    :<|> "alter" :> Capture "article_id" ArticleId
      :> ( "text" :> MultipartForm Mem Article :> PostNoContent
             :<|> "images"
               :> ( MultipartForm Mem (MultipartData Mem) :> Post '[JSON] (WithStatus 202 [ImageId]) -- returns all image_ids of new images
                      :<|> QueryParams "ids" [ImageId] :> Delete '[JSON] (WithStatus 202 Int) -- returns number of deleted images
                  )
         )
    :<|> ("get" :> ArticleFilters :> QueryParam "sort_by" SortBy :> Get '[JSON] [InternalArticle])

data InternalArticle = InternalArticle
  { iArticleId :: ArticleId,
    iArticleTitle :: String,
    iArticleUser :: InternalUser,
    iArticleCategory :: NestCategory,
    iArticleContent :: String,
    iArticleImages :: [ImageId]
  }

data InternalUser = InternalUser
  { iUserId :: UserId,
    iUserName :: String,
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

data IncomingUser = IncomingUser {name :: String, password :: String, isAdmin_ :: Bool, isAuthor_ :: Bool}

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
