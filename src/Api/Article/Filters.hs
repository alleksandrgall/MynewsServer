{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Api.Article.Filters where

import Api.Internal.Pagination
import DB.Scheme
import Data.Time
import Database.Esqueleto.Experimental
import Servant

type GetWithFilters (t :: [*]) a =
  QueryParam "created_since" Day :> QueryParam "created_until" Day :> QueryParam "created_at" Day
    :> QueryParam "author" String
    :> QueryParam "category" CategoryId
    :> QueryParam "title_has" String
    :> QueryParam "content_has" String
    -- API новостей должно поддерживать поиск по строке,
    --которая может быть найдена либо в текстовом контенте, либо в имени автора, либо в названии категории.
    :> QueryParam "search" String
    :> QueryParam "sort_by" SortBy
    :> GetWithPagination t a

maybeFilter :: Maybe a -> (a -> SqlExpr a) -> SqlExpr a
maybeFilter Nothing _ = x
maybeFilter (Just p) f = f p

createdSince :: SqlExpr (Entity Article) -> Day -> SqlExpr (Value Bool)
createdSince art d = art ^. ArticleCreated >. val d

createdUntil :: SqlExpr (Entity Article) -> Day -> SqlQuery ()
createdUntil art d = where_ (art ^. ArticleCreated <. val d)

data SortBy = Date | Author | Category_ | ImageNum

-- instance ToHttpApiData SortBy where
--   toUrlPiece Date = "date"
--   toUrlPiece Author = "author"
--   toUrlPiece Category_ = "category"
--   toUrlPiece ImageNum = "image_number"

instance FromHttpApiData SortBy where
  parseUrlPiece t
    | t == "date" = Right Date
    | t == "author" = Right Author
    | t == "category" = Right Category_
    | t == "image_number" = Right ImageNum
    | otherwise = Left "Use one: \"date\", \"author\", \"category\", \"image_number\""
