{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use unless" #-}

module Api.Category where

import Api.Auth
import Api.Pagination
import App
import Control.Exception (Exception, SomeException (SomeException))
import Control.Monad (when)
import Control.Monad.Catch hiding (Handler)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (liftIO)
import DB.Scheme
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Text as T
import Database.Esqueleto.Experimental hiding (Nothing, isNothing)
import qualified Database.Persist.Sql as P
import Dev
import Servant
import SqlException (SqlException (NotExists))

type CategoryApi =
  BasicAuth "admin" (Entity User)
    :> ( "create" :> QueryParam' [Required, Strict] "name" String
           :> QueryParam' [Required, Strict] "parent" Parent
           :> Put '[JSON] CategoryId
       )
    :<|> BasicAuth "admin" (Entity User)
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

data QParam = forall a. QParam (Maybe a)

allNothing :: [QParam] -> Bool
allNothing = all (\(QParam x) -> isNothing x)

data MaybeSetter v = forall typ. PersistField typ => MaybeSetter (EntityField v typ, Maybe typ)

setsMaybe :: [MaybeSetter v] -> [P.Update v]
setsMaybe [] = []
setsMaybe (MaybeSetter (_, Nothing) : fvs) = setsMaybe fvs
setsMaybe (MaybeSetter (f, Just v) : fvs) = (f P.=. v) : setsMaybe fvs

isUniqueSibling :: String -> Parent -> App Bool
isUniqueSibling name parentId =
  runDBDev $
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

create :: Entity User -> String -> Parent -> App CategoryId
create u name parentId = do
  userIsAdmin_ u
  isUniqueSibling name parentId >>= \f ->
    when
      (not f)
      $ throwError err400 {errReasonPhrase = "Already have a category with the same \"name\" and \"parent\"."}

  runDBDev (insert $ Category name (unParent parentId)) `catch` categoryDBHandler

alter :: Entity User -> CategoryId -> Maybe String -> Maybe Parent -> App NoContent
alter u trgId name parentId = do
  userIsAdmin_ u
  when (allNothing [QParam name, QParam parentId]) $
    throwError err400 {errReasonPhrase = "Query parameter \"name\" and/or \"parent\" are required"}
  oldCateg <- runDBDev (get trgId) >>= maybe (throwError err400 {errReasonPhrase = "Given category doesn't exist."}) return
  let isSelfParent = maybe False ((Just trgId ==) . unParent) parentId
  when isSelfParent $ throwError err400 {errReasonPhrase = "Category is a parent of itself."}
  isUniqueSibling (fromMaybe (categoryName oldCateg) name) (fromMaybe (Parent $ categoryParent oldCateg) parentId) >>= \f ->
    when (not f) $ throwError err400 {errReasonPhrase = "Already have a category with the same \"name\" and \"parent\"."}

  (runDBDev . P.update trgId $ setsMaybe [MaybeSetter (CategoryName, name), MaybeSetter (CategoryParent, unParent <$> parentId)])
    `catch` categoryDBHandler
  return NoContent

getC :: Maybe Limit -> Maybe Offset -> App (WithOffset [Entity Category])
getC lim off = do
  runDBDev
    . selectPagination lim off
    $ do
      from $ table @Category
