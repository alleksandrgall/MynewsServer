{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Api.Category where

import Api.Internal.Auth (userIsAdmin_)
import Api.Internal.Optional (MaybeSetter (MaybeSetter), QParam (QParam), allNothing, setsMaybe)
import Api.Internal.Pagination (GetWithPagination, Limit, Offset, WithOffset, selectPagination)
import App.App (App, askPaginationLimit, runDB)
import App.Auth (Auth (..))
import Control.Monad (when)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import DB.Scheme
  ( Category (..),
    CategoryId,
    EntityField (CategoryName, CategoryParent),
  )
import Data.Bool (bool)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import Database.Esqueleto.Experimental
  ( Entity,
    PersistStoreRead (get),
    PersistStoreWrite (insert),
    from,
    select,
    table,
    val,
    where_,
    (&&.),
    (==.),
    (^.),
  )
import qualified Database.Persist.Sql as P
import Katip (Severity (InfoS), katipAddContext, logFM, sl)
import Servant
  ( AuthProtect,
    Capture,
    FromHttpApiData (parseUrlPiece),
    HasServer (ServerT),
    JSON,
    NoContent (..),
    PostNoContent,
    Proxy (..),
    Put,
    QueryParam,
    QueryParam',
    Required,
    ServerError (errReasonPhrase),
    Strict,
    err400,
    throwError,
    type (:<|>) (..),
    type (:>),
  )

type CategoryApi =
  AuthProtect "admin"
    :> ( "create" :> QueryParam' [Required, Strict] "name" String
           :> QueryParam' [Required, Strict] "parent" Parent
           :> Put '[JSON] CategoryId
       )
    :<|> AuthProtect "admin"
    :> ( "alter" :> Capture "category_id" CategoryId
           :> QueryParam "name" String
           :> QueryParam "parent" Parent
           :> PostNoContent
       )
    :<|> GetWithPagination '[JSON] (Entity Category)

categoryApi :: Proxy CategoryApi
categoryApi = Proxy

categoryServer :: ServerT CategoryApi App
categoryServer = create :<|> alter :<|> getC

newtype Parent = Parent {unParent :: Maybe CategoryId}

instance FromHttpApiData Parent where
  parseUrlPiece s
    | T.toLower s == "root" = Right $ Parent Nothing
    | otherwise = Parent . Just <$> parseUrlPiece @CategoryId s

isUniqueSibling :: String -> Parent -> ExceptT String P.SqlPersistM ()
isUniqueSibling name parentId =
  ExceptT $
    bool (Left "Already have a category with the same \"name\" and \"parent\".") (Right ()) . null
      <$> ( select $ do
              c <- from $ table @Category
              where_ (c ^. CategoryParent ==. val (unParent parentId) &&. c ^. CategoryName ==. val name)
              pure c
          )

isParentExists :: Parent -> ExceptT String P.SqlPersistM ()
isParentExists (Parent Nothing) = return ()
isParentExists (Parent (Just catId)) = ExceptT $ bool (Left "Given parent category doesn't exist.") (Right ()) . isJust <$> P.get catId

create :: Auth a -> String -> Parent -> App CategoryId
create (Auth u) name parentId = do
  logFM InfoS "Creating a category"
  userIsAdmin_ u
  either (\e -> throwError err400 {errReasonPhrase = e}) return
    =<< ( runDB . runExceptT $ do
            isUniqueSibling name parentId
            isParentExists parentId
        )
  catId <- runDB (insert $ Category name (unParent parentId))
  katipAddContext (sl "cateogory_id" catId) $ logFM InfoS "Category created" >> return catId

alter :: Auth a -> CategoryId -> Maybe String -> Maybe Parent -> App NoContent
alter (Auth u) trgId name parentId = do
  katipAddContext (sl "cateogory_id" trgId) $ do
    logFM InfoS "Altering cateogry"
    userIsAdmin_ u
    when (allNothing [QParam name, QParam parentId]) $
      throwError err400 {errReasonPhrase = "Query parameter \"name\" and/or \"parent\" are required"}
    oldCateg <- runDB (get trgId) >>= maybe (throwError err400 {errReasonPhrase = "Given category doesn't exist."}) return
    let isSelfParent = maybe False ((Just trgId ==) . unParent) parentId
    when isSelfParent $ throwError err400 {errReasonPhrase = "Category is a parent of itself."}
    either (\e -> throwError err400 {errReasonPhrase = e}) return
      =<< ( runDB . runExceptT $ do
              isUniqueSibling (fromMaybe (categoryName oldCateg) name) (fromMaybe (Parent $ categoryParent oldCateg) parentId)
              isParentExists (fromMaybe (Parent $ categoryParent oldCateg) parentId)
          )
    runDB . P.update trgId $ setsMaybe [MaybeSetter (CategoryName, name), MaybeSetter (CategoryParent, unParent <$> parentId)]
    logFM InfoS "Category altered"
    return NoContent

getC :: Maybe Limit -> Maybe Offset -> App (WithOffset (Entity Category))
getC lim off = do
  katipAddContext (sl "limit" lim <> sl "offset" off) $ do
    logFM InfoS "Sending categories"
    maxLimit <- askPaginationLimit
    cats <-
      runDB
        . selectPagination lim off maxLimit
        $ from $ table @Category
    katipAddContext (sl "cateogry_number" (length cats)) $ logFM InfoS "Categories sent" >> return cats
