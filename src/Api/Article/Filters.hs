{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Api.Article.Filters where

import Api.Internal.Pagination (GetWithPagination)
import DB.Scheme
  ( Article,
    Category,
    CategoryId,
    EntityField
      ( ArticleContent,
        ArticleCreated,
        ArticleTitle,
        CategoryId,
        CategoryName,
        UserName
      ),
    User,
  )
import Data.Time (Day)
import Database.Esqueleto.Experimental
  ( Entity,
    OrderBy,
    SqlExpr,
    Value,
    asc,
    like,
    val,
    (%),
    (++.),
    (<.),
    (==.),
    (>.),
    (^.),
    (||.),
  )
import Servant (FromHttpApiData (parseUrlPiece), QueryParam, type (:>))

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

maybeFilter :: Maybe a -> (a -> SqlExpr (Value Bool)) -> SqlExpr (Value Bool)
maybeFilter filterParam f = maybe (val True) f filterParam

createdSinceF :: SqlExpr (Entity Article) -> Day -> SqlExpr (Value Bool)
createdSinceF art d = art ^. ArticleCreated >. val d

createdUntilF :: SqlExpr (Entity Article) -> Day -> SqlExpr (Value Bool)
createdUntilF art d = art ^. ArticleCreated <. val d

createdAtF :: SqlExpr (Entity Article) -> Day -> SqlExpr (Value Bool)
createdAtF art d = art ^. ArticleCreated ==. val d

authorNameF :: SqlExpr (Entity User) -> String -> SqlExpr (Value Bool)
authorNameF user_ login = user_ ^. UserName ==. val login

categoryIdF_ :: SqlExpr (Entity Category) -> CategoryId -> SqlExpr (Value Bool)
categoryIdF_ cat catId = cat ^. CategoryId ==. val catId

titleHasF :: SqlExpr (Entity Article) -> String -> SqlExpr (Value Bool)
titleHasF art subStr = art ^. ArticleTitle `like` (%) ++. val subStr ++. (%)

contentHasF :: SqlExpr (Entity Article) -> String -> SqlExpr (Value Bool)
contentHasF art subStr = art ^. ArticleContent `like` (%) ++. val subStr ++. (%)

searchF :: SqlExpr (Entity Article) -> SqlExpr (Entity User) -> SqlExpr (Entity Category) -> String -> SqlExpr (Value Bool)
searchF art user_ cat subStr =
  (art ^. ArticleContent `like` (%) ++. val subStr ++. (%)) ||. (user_ ^. UserName `like` (%) ++. val subStr ++. (%))
    ||. (cat ^. CategoryName `like` (%) ++. val subStr ++. (%))

maybeSort :: Maybe SortBy -> (SortBy -> [SqlExpr OrderBy]) -> [SqlExpr OrderBy]
maybeSort sortParam f = maybe [] f sortParam

sortByF_ :: SqlExpr (Entity Article) -> SqlExpr (Entity User) -> SqlExpr (Entity Category) -> SqlExpr (Value Int) -> SortBy -> [SqlExpr OrderBy]
sortByF_ art _ _ _ Date = [asc (art ^. ArticleCreated)]
sortByF_ _ user_ _ _ Author = [asc (user_ ^. UserName)]
sortByF_ _ _ cat _ Category_ = [asc (cat ^. CategoryName)]
sortByF_ _ _ _ imageNum ImageNum = [asc imageNum]

data SortBy = Date | Author | Category_ | ImageNum deriving (Show)

instance FromHttpApiData SortBy where
  parseUrlPiece t
    | t == "date" = Right Date
    | t == "author" = Right Author
    | t == "category" = Right Category_
    | t == "image_number" = Right ImageNum
    | otherwise = Left "Use one: \"date\", \"author\", \"category\", \"image_number\""
