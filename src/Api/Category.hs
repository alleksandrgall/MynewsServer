{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Api.Category where

import Api.Internal.Auth
import Api.Internal.Optional
import Api.Internal.Pagination
import App
import Control.Exception (Exception, SomeException (SomeException))
import Control.Monad (unless, when)
import Control.Monad.Catch hiding (Handler)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (liftIO)
import DB.Scheme
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Text as T
import Database.Esqueleto.Experimental hiding (Nothing, isNothing)
import qualified Database.Persist.Sql as P
import Katip (Severity (InfoS), katipAddContext, logFM, sl)
import Servant
import SqlException (SqlException (NotExists))

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

isUniqueSibling :: String -> Parent -> App Bool
isUniqueSibling name parentId =
  runDB $
    null
      <$> ( select $ do
              c <- from $ table @Category
              where_ (c ^. CategoryParent ==. val (unParent parentId) &&. c ^. CategoryName ==. val name)
              pure c
          )

categoryDBHandler :: (MonadError ServerError m, MonadThrow m) => SomeException -> m a
categoryDBHandler e = case fromException e of
  Just NotExists -> throwError err400 {errReasonPhrase = "Given parent category doesn't exist."}
  _ -> throwM e

create :: Auth a -> String -> Parent -> App CategoryId
create (Auth u) name parentId = do
  logFM InfoS "Creating a category"
  userIsAdmin_ u
  isUniqueSibling name parentId >>= \f ->
    unless f $ throwError err400 {errReasonPhrase = "Already have a category with the same \"name\" and \"parent\"."}
  catId <- runDB (insert $ Category name (unParent parentId)) `catch` categoryDBHandler
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
    isUniqueSibling (fromMaybe (categoryName oldCateg) name) (fromMaybe (Parent $ categoryParent oldCateg) parentId) >>= \f ->
      unless f $ throwError err400 {errReasonPhrase = "Already have a category with the same \"name\" and \"parent\"."}
    (runDB . P.update trgId $ setsMaybe [MaybeSetter (CategoryName, name), MaybeSetter (CategoryParent, unParent <$> parentId)])
      `catch` categoryDBHandler
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
