{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# HLINT ignore "Fuse on/on" #-}

module DB.Queries where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT)
import DB.Scheme
import DB.Scheme (Unique (UniqueUserName))
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.List (find)
import qualified Data.Map as M
import Data.Maybe (isNothing)
import Database.Esqueleto.Experimental hiding (insert, isNothing)
import Database.Persist (insert)

queryNestCategoryById :: CategoryId -> SqlQuery (SqlExpr (Entity Category))
queryNestCategoryById catId = do
  categories <-
    withRecursive
      ( do
          cat <- from $ table @Category
          where_ $ cat ^. CategoryId ==. val catId
          pure cat
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
  from categories

type Limit = Int64

type Offset = Int64

queryInsertUser :: User -> SqlPersistM UserId
queryInsertUser = insert

queryPromoteToAuthor :: String -> SqlPersistM ()
queryPromoteToAuthor name = update $ \u -> do
  set u [UserIsAuthor =. val True]
  where_ $ u ^. UserName ==. val name

queryCategoryList :: SqlQuery (SqlExpr (Entity Category))
queryCategoryList = from $ table @Category

addPagination :: Limit -> Offset -> SqlQuery (SqlExpr a) -> SqlQuery (SqlExpr a)
addPagination lim off q = q >>= \r -> limit lim >> offset off >> pure r

addFilter :: (SqlExpr a -> SqlExpr (Value Bool)) -> SqlQuery (SqlExpr a) -> SqlQuery (SqlExpr a)
addFilter p q = q >>= \r -> where_ (p r) >> pure r

addArticleSearch ::
  String ->
  SqlQuery (SqlExpr (Entity Article) :& SqlExpr (Entity User) :& SqlExpr (Entity Category)) ->
  SqlQuery (SqlExpr (Entity Article) :& SqlExpr (Entity User) :& SqlExpr (Entity Category))
addArticleSearch substr q = do
  (a :& u :& c) <- q
  where_ $
    (a ^. ArticleContent `like` val ("%" <> substr <> "%"))
      ||. (u ^. UserName `like` val ("%" <> substr <> "%"))
      ||. c ^. CategoryName `like` val ("%" <> substr <> "%")
  pure (a :& u :& c)

-- queryArticles :: SqlQuery (SqlExpr (Entity Article), SqlExpr (Entity User), SqlExpr (Entity Category), SqlExpr (Value ImageId))
-- queryArticles = do
--   (a :& u :& c :& im) <-
--     from $
--       table @Article
--         `innerJoin` table @User
--         `on` (\(a :& u) -> a ^. ArticleUserId ==. u ^. UserId)
--         `innerJoin` table @Category
--         `on` (\(a :& _ :& c) -> a ^. ArticleCategoryId ==. c ^. CategoryId)
--         `innerJoin` table @Image
--         `on` (\(a :& _ :& _ :& im) -> im ^. ImageArticleId ==. a ^. ArticleId)
--   groupBy (a ^. ArticleId)
--   pure (a, u, c, im ^. ImageId)

-- f :: SqlPersistM [(Entity Article, Entity User, Entity Category, Value ImageId)]
-- f = select queryArticles