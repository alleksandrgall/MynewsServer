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

createdSinceF :: SqlExpr (Entity Article) -> Maybe Day -> SqlExpr (Value Bool)
createdSinceF art d = maybeFilter d (\d' -> art ^. ArticleCreated >. val d')

createdUntilF :: SqlExpr (Entity Article) -> Maybe Day -> SqlExpr (Value Bool)
createdUntilF art d = maybeFilter d (\d' -> art ^. ArticleCreated <. val d')

createdAtF :: SqlExpr (Entity Article) -> Maybe Day -> SqlExpr (Value Bool)
createdAtF art d = maybeFilter d (\d' -> art ^. ArticleCreated ==. val d')

authorNameF :: SqlExpr (Entity User) -> Maybe String -> SqlExpr (Value Bool)
authorNameF user_ login = maybeFilter login (\login' -> user_ ^. UserName ==. val login')

categoryIdF_ :: SqlExpr (Entity Category) -> Maybe CategoryId -> SqlExpr (Value Bool)
categoryIdF_ cat catId = maybeFilter catId (\catId' -> cat ^. CategoryId ==. val catId')

titleHasF :: SqlExpr (Entity Article) -> Maybe String -> SqlExpr (Value Bool)
titleHasF art subStr = maybeFilter subStr (\subStr' -> art ^. ArticleTitle `like` (%) ++. val subStr' ++. (%))

contentHasF :: SqlExpr (Entity Article) -> Maybe String -> SqlExpr (Value Bool)
contentHasF art subStr = maybeFilter subStr (\subStr' -> art ^. ArticleContent `like` (%) ++. val subStr' ++. (%))

searchF :: SqlExpr (Entity Article) -> SqlExpr (Entity User) -> SqlExpr (Entity Category) -> Maybe String -> SqlExpr (Value Bool)
searchF art user_ cat subStr = maybeFilter subStr $ \subStr' ->
  (art ^. ArticleContent `like` (%) ++. val subStr' ++. (%)) ||. (user_ ^. UserName `like` (%) ++. val subStr' ++. (%))
    ||. (cat ^. CategoryName `like` (%) ++. val subStr' ++. (%))

sortByF_ :: SqlExpr (Entity Article) -> SqlExpr (Entity User) -> SqlExpr (Entity Category) -> SqlExpr (Value Int) -> Maybe SortBy -> [SqlExpr OrderBy]
sortByF_ art _ _ _ (Just Date) = [asc (art ^. ArticleCreated)]
sortByF_ _ user_ _ _ (Just Author) = [asc (user_ ^. UserName)]
sortByF_ _ _ cat _ (Just Category_) = [asc (cat ^. CategoryName)]
sortByF_ _ _ _ imageNum (Just ImageNum) = [asc imageNum]
sortByF_ _ _ _ _ Nothing = []

data SortBy = Date | Author | Category_ | ImageNum deriving (Show)

instance FromHttpApiData SortBy where
  parseUrlPiece t
    | t == "date" = Right Date
    | t == "author" = Right Author
    | t == "category" = Right Category_
    | t == "image_number" = Right ImageNum
    | otherwise = Left "Use one: \"date\", \"author\", \"category\", \"image_number\""
