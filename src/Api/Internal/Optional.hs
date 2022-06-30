{-# LANGUAGE ExistentialQuantification #-}

module Api.Internal.Optional where

import Data.Maybe (isNothing)
import qualified Data.Text as T
import qualified Database.Persist.Sql as P
import Handlers.DB.Scheme (Article, EntityField (ArticleCategoryId, ArticleContent, ArticleIsPublished, ArticleTitle), Key (CategoryKey))
import Servant.Multipart (FromMultipart (fromMultipart), Mem, lookupInput)
import qualified Text.Read as T

data QParam = forall a. QParam (Maybe a)

allNothing :: [QParam] -> Bool
allNothing = all (\(QParam x) -> isNothing x)

data MaybeSetter v = forall typ. P.PersistField typ => MaybeSetter (EntityField v typ, Maybe typ)

setsMaybe :: [MaybeSetter v] -> [P.Update v]
setsMaybe [] = []
setsMaybe (MaybeSetter (_, Nothing) : ss) = setsMaybe ss
setsMaybe (MaybeSetter (f, Just v) : ss) = (f P.=. v) : setsMaybe ss

instance FromMultipart Mem [MaybeSetter Article] where
  fromMultipart form =
    Right
      [ MaybeSetter (ArticleTitle, lookupF (Just . T.unpack) "title"),
        MaybeSetter (ArticleCategoryId, lookupF (fmap (CategoryKey . P.SqlBackendKey) . T.readMaybe . T.unpack) "category_id"),
        MaybeSetter (ArticleContent, lookupF (Just . T.unpack) "content"),
        MaybeSetter (ArticleIsPublished, lookupF (T.readMaybe . T.unpack) "is_published")
      ]
    where
      lookupF :: (T.Text -> Maybe a) -> T.Text -> Maybe a
      lookupF f name = either (const Nothing) f (lookupInput name form)