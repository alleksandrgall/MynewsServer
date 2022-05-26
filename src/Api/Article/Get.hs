{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Fuse on/on" #-}

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
import Data.Aeson
  ( KeyValue ((.=)),
    Options (fieldLabelModifier),
    ToJSON (toJSON),
    camelTo2,
    defaultOptions,
    genericToJSON,
    object,
  )
import Data.Function ((&))
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time
import Database.Esqueleto.Experimental
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

instance ToJSON FormatArticle where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 8}

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
  (SqlExpr (Entity Article) -> SqlExpr (Entity User) -> SqlExpr (Entity Category) -> SqlExpr (Value Int) -> SqlExpr (Value Bool)) ->
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
    where_ (filters art us cat countImages &&. art ^. ArticleIsPublished)
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
            (cSelf :& c) <-
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