{-# LANGUAGE DeriveGeneric #-}
{-# HLINT ignore "Fuse on/on" #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Api.Article.Get
  ( FormatArticle (..),
    NestCategory (..),
    getFormatArticle,
    getFormatArticlesPagination,
    parseListToNest,
  )
where

import Api.Internal.Pagination (Limit, Offset, WithOffset, selectPagination)
import Api.User (FormatUser, formatEntityUser)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty ((:|)))
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
    selectOne,
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
import GHC.Natural (Natural)
import Handlers.DB.Scheme
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

data FormatArticle = FormatArticle
  { formatArticleId :: ArticleId,
    formatArticleTitle :: String,
    formatArticleCreated :: Day,
    formatArticleUser :: FormatUser,
    formatArticleCategory :: NestCategory,
    formatArticleIsPublished :: Bool,
    formatArticleContent :: String,
    formatArticleImages :: [ImageId]
  }
  deriving (Generic, Show, Eq)

instance A.FromJSON FormatArticle where
  parseJSON = A.genericParseJSON A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_' . drop 13}

instance A.ToJSON FormatArticle where
  toJSON = A.genericToJSON A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_' . drop 13}

data NestCategory = NestCategory CategoryId String NestCategory | Last CategoryId String
  deriving (Show, Eq)

instance A.FromJSON NestCategory where
  parseJSON (A.Object o) = do
    maybeNext <- o A..:? "category_sub" :: A.Parser (Maybe NestCategory)
    case maybeNext of
      Nothing -> Last <$> o A..: "id" <*> o A..: "category_name"
      (Just next) -> NestCategory <$> o A..: "id" <*> o A..: "category_name" <&> ($ next)
  parseJSON _ = mempty

instance A.ToJSON NestCategory where
  toJSON (Last i name) =
    A.object
      [ "id" A..= i,
        "category_name" A..= T.pack name
      ]
  toJSON (NestCategory i name children) =
    A.object
      [ "id" A..= i,
        "category_name" A..= T.pack name,
        "category_sub" A..= A.toJSON children
      ]

parseListToNest :: NonEmpty (P.Entity Category) -> NestCategory
parseListToNest (mainEnt :| restEnts) =
  let mEnt = M.fromList $ map (\ent -> (categoryParent . P.entityVal $ ent, ent)) restEnts
      root = M.lookup Nothing mEnt
      children Nothing = Last (entityKey mainEnt) (categoryName . entityVal $ mainEnt)
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
  Natural ->
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
  restCategories <- select $ do
    from
      =<< withRecursive
        ( do
            firstParent <- from $ table @Category
            where_ $ just (firstParent ^. CategoryId) ==. val (categoryParent . entityVal $ cat)
            pure firstParent
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
  let categories = cat :| restCategories
  images_ <- select $ from (table @ImageArticle) >>= \imArt -> where_ (imArt ^. ImageArticleArticleId ==. val (entityKey art)) >> return imArt
  let images = fmap (imageArticleImageId . entityVal) images_
  return $
    FormatArticle
      { formatArticleId = entityKey art,
        formatArticleTitle = articleTitle . entityVal $ art,
        formatArticleCreated = articleCreated . entityVal $ art,
        formatArticleUser = formatEntityUser us,
        formatArticleCategory = parseListToNest categories,
        formatArticleIsPublished = articleIsPublished . entityVal $ art,
        formatArticleContent = articleContent . entityVal $ art,
        formatArticleImages = images
      }