{-# LANGUAGE DeriveGeneric #-}
{-# HLINT ignore "Fuse on/on" #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Api.Article.Get
  ( FormatArticle,
    getFormatArticle,
    getFormatArticlesPagination,
  )
where

import Api.Internal.Pagination (Limit, Offset, WithOffset, selectPagination)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import DB.Scheme
  ( Article
      ( articleCategoryId,
        articleContent,
        articleCreated,
        articleIsPublished,
        articleTitle,
        articleUserId
      ),
    ArticleId,
    Category (categoryName, categoryParent),
    CategoryId,
    EntityField
      ( ArticleCategoryId,
        ArticleId,
        ArticleIsPublished,
        ArticleUserId,
        CategoryId,
        CategoryParent,
        ImageArticleArticleId,
        UserId
      ),
    ImageArticle (imageArticleImageId),
    ImageId,
    User,
  )
import qualified Data.Aeson as A
import Data.Function ((&))
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time (Day)
import Database.Esqueleto.Experimental
  ( Entity (entityKey, entityVal),
    OrderBy,
    SqlExpr,
    SqlPersistM,
    Value,
    from,
    innerJoin,
    just,
    on,
    orderBy,
    select,
    subSelectCount,
    table,
    union_,
    val,
    where_,
    withRecursive,
    (&&.),
    (==.),
    (^.),
    type (:&) ((:&)),
  )
import qualified Database.Persist as P
import GHC.Generics (Generic)

data FormatArticle = FormatArticle
  { iArticleId :: ArticleId,
    iArticleTitle :: String,
    iArticleCreated :: Day,
    iArticleUser :: P.Entity User,
    iArticleCategory :: NestCategory,
    iArticleIsPublished :: Bool,
    iArticleContent :: String,
    iArticleImages :: [ImageId]
  }
  deriving (Generic)

instance A.ToJSON FormatArticle where
  toJSON = A.genericToJSON A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_' . drop 8}

data NestCategory = NestCategory CategoryId String NestCategory | Non
  deriving (Show)

instance A.ToJSON NestCategory where
  toJSON Non = A.object []
  toJSON (NestCategory i name children) =
    A.object
      [ "id" A..= i,
        "category_name" A..= T.pack name,
        "category_sub" A..= A.toJSON children
      ]

parseListToNest :: [P.Entity Category] -> NestCategory
parseListToNest ents =
  let mEnt = M.fromList $ map (\ent -> (categoryParent . P.entityVal $ ent, ent)) ents
      root = M.lookup Nothing mEnt
      children Nothing = Non
      children (Just c) =
        NestCategory (P.entityKey c) (categoryName . P.entityVal $ c) $ children . M.lookup (Just $ P.entityKey c) $ mEnt
   in children root

getFormatArticle :: ArticleId -> SqlPersistM (Maybe FormatArticle)
getFormatArticle aId = runMaybeT $ do
  art <- MaybeT $ P.getEntity aId
  author <- MaybeT $ P.getEntity (art & entityVal & articleUserId)
  category <- MaybeT $ P.getEntity (art & entityVal & articleCategoryId)
  lift $ toFormatArticle (art, author, category)

getFormatArticlesPagination ::
  (SqlExpr (Entity Article) -> SqlExpr (Entity User) -> SqlExpr (Entity Category) -> SqlExpr (Value Bool)) ->
  (SqlExpr (Entity Article) -> SqlExpr (Entity User) -> SqlExpr (Entity Category) -> SqlExpr (Value Int) -> [SqlExpr OrderBy]) ->
  Maybe Limit ->
  Int ->
  Maybe Offset ->
  SqlPersistM (WithOffset FormatArticle)
getFormatArticlesPagination filters order limit_ maxLimit offset_ = do
  selected <- selectPagination limit_ offset_ maxLimit $ do
    (art :& us :& cat) <-
      from $
        table @Article
          `innerJoin` table @User
          `on` ( \(art :& us) ->
                   art ^. ArticleUserId ==. us ^. UserId
               )
          `innerJoin` table @Category
          `on` ( \(art :& _ :& cat) ->
                   art ^. ArticleCategoryId ==. cat ^. CategoryId
               )
    let countImages = subSelectCount $ do
          imArt <- from $ table @ImageArticle
          where_ (imArt ^. ImageArticleArticleId ==. art ^. ArticleId)
          pure imArt
    where_ (filters art us cat &&. art ^. ArticleIsPublished)
    orderBy (order art us cat countImages)
    pure (art, us, cat)
  mapM toFormatArticle selected

toFormatArticle :: (Entity Article, Entity User, Entity Category) -> SqlPersistM FormatArticle
toFormatArticle (art, us, cat) = do
  categories <- select $ do
    from
      =<< withRecursive
        ( do
            cat_ <- from $ table @Category
            where_ $ cat_ ^. CategoryId ==. val (entityKey cat)
            pure cat_
        )
        union_
        ( \self -> do
            (_ :& c) <-
              from $
                self
                  `innerJoin` table @Category
                  `on` \(cSelf :& c) ->
                    cSelf ^. CategoryParent ==. just (c ^. CategoryId)
            pure c
        )
  images_ <- select $ from (table @ImageArticle) >>= \imArt -> where_ (imArt ^. ImageArticleArticleId ==. val (entityKey art)) >> return imArt
  let images = fmap (imageArticleImageId . entityVal) images_
  return $
    FormatArticle
      { iArticleId = entityKey art,
        iArticleTitle = articleTitle . entityVal $ art,
        iArticleCreated = articleCreated . entityVal $ art,
        iArticleUser = us,
        iArticleCategory = parseListToNest categories,
        iArticleIsPublished = articleIsPublished . entityVal $ art,
        iArticleContent = articleContent . entityVal $ art,
        iArticleImages = images
      }