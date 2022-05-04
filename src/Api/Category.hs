{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# HLINT ignore "Use unless" #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Api.Category where

import Api
import Api.Pagination
import Control.Exception (Exception, SomeException (SomeException))
import Control.Monad (guard, liftM2, when)
import Control.Monad.Catch hiding (Handler)
import Control.Monad.IO.Class (liftIO)
import DB.Scheme
import Data.Bool (bool)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Database.Esqueleto.Experimental hiding (Nothing, isNothing)
import qualified Database.Persist.Sql as P
import Dev
import Servant

data Root = Root

instance FromHttpApiData Root where
  parseUrlPiece "root" = Right Root
  parseUrlPiece _ = Left "Use root"

type CategoryApi =
  BasicAuth "admin" User
    :> ( "create" :> QueryParam' [Required, Strict] "name" String
           :> QueryParam' [Required, Strict] "parent" (Either Root CategoryId)
           :> Put '[JSON] CategoryId
       )
    :<|> BasicAuth "admin" User
    :> ( "alter" :> Capture "category_id" CategoryId
           :> QueryParam "name" String
           :> QueryParam "parent" (Either Root CategoryId)
           :> Post '[] NoContent
       )
    :<|> ("get" :> QueryParam "offset" Int :> QueryParam "limit" Int :> Get '[JSON] [Category])

data DBException = AlreadyExists | NotExists deriving (Show, Exception)

create :: User -> String -> Either Root CategoryId -> Handler CategoryId
create u name parentId = do
  userIsAdmin_ u
  let unEitherParentId = either (const Nothing) Just parentId
  runDBDev
    ( do
        isParentExists <- case unEitherParentId of
          Nothing -> return True
          Just i -> isJust <$> get i
        when (not isParentExists) $ throwM NotExists
        isUniqueSibling <-
          null
            <$> ( select $ do
                    c <- from $ table @Category
                    where_ (c ^. CategoryParent ==. val unEitherParentId &&. c ^. CategoryName ==. val name)
                    pure c
                )
        when (not isUniqueSibling) $ throwM AlreadyExists
        insert $ Category name unEitherParentId
    )
    `catch` ( \case
                NotExists -> throwError err400 {errReasonPhrase = "Given \"parent\" category doesn't exist."}
                AlreadyExists -> throwError err400 {errReasonPhrase = "Already have a category with the same \"name\" and \"parent\"."}
            )

makeSet ::
  (PersistEntity val, PersistField a) =>
  Maybe a ->
  (a -> SqlPersistM Bool) ->
  ServerError ->
  (a -> SqlExpr (Entity val) -> SqlQuery ()) ->
  Handler (SqlExpr (Entity val) -> SqlQuery ())
makeSet maybeField p err updater = do
  case maybeField of
    Nothing -> return $ const (return ())
    (Just field) -> do
      f <- runDBDev (p field)
      when (not f) $ throwError err
      return undefined

alter :: User -> CategoryId -> Maybe String -> Maybe (Either Root CategoryId) -> Handler CategoryId
--alter _ _ Nothing Nothing = throwError err400 {errReasonPhrase = "Query parameter \"name\" and/or \"parent\" is required"}
alter u trgId name parentId = do
  userIsAdmin_ u
  when (isNothing name && isNothing parentId) (throwError err400 {errReasonPhrase = "Query parameter \"name\" and/or \"parent\" is required"})
  categ <- runDBDev (get trgId) >>= maybe (throwError err400 {errReasonPhrase = "Given category doesn't exist."}) return
  let newCateg =
        Category
          (fromMaybe (categoryName categ) name)
          (maybe (categoryParent categ) (either (const Nothing) Just) parentId)
  isParentExists <- runDBDev $
    fmap (bool (categoryParent categ == categoryParent newCateg) True) $ do
      case categoryParent newCateg of
        Nothing -> return True
        Just i -> isJust <$> get i
  when (not isParentExists) $ throwError err400 {errReasonPhrase = "Given \"parent\" category doesn't exist."}
  isUniqueSibling <-
    runDBDev $
      null
        <$> ( select $ do
                c <- from $ table @Category
                where_ (c ^. CategoryParent ==. val (categoryParent newCateg) &&. c ^. CategoryName ==. val (categoryName newCateg))
                pure c
            )
  when (not isUniqueSibling) $ throwError err400 {errReasonPhrase = "Already have a category with the same \"name\" and \"parent\"."}

  return undefined

alter' :: User -> CategoryId -> Maybe String -> Maybe (Either Root CategoryId) -> Handler NoContent
alter' u trgId name parentId = do
  userIsAdmin_ u
  categ <- runDBDev (get trgId) >>= maybe (throwError err400 {errReasonPhrase = "Given category doesn't exist."}) return
  helper trgId categ name parentId
  where
    helper _ _ Nothing Nothing = throwError err400 {errReasonPhrase = "Query parameter \"name\" and/or \"parent\" is required"}
    helper trgId categ (Just name) Nothing = do
      isUniqueSibling <-
        runDBDev $
          null
            <$> ( select $ do
                    c <- from $ table @Category
                    where_ (c ^. CategoryParent ==. val (categoryParent categ) &&. c ^. CategoryName ==. val name)
                    pure c
                )
      when (not isUniqueSibling) $ throwError err400 {errReasonPhrase = "Already have a category with the same \"name\" and \"parent\"."}
      runDBDev $ P.update trgId [CategoryName P.=. name]
      return NoContent
    helper trgId categ Nothing (Just parentId) = do
      let unEitherParentId = either (const Nothing) Just parentId
      isParentExists <- runDBDev $ do
        case unEitherParentId of
          Nothing -> return True
          Just i -> isJust <$> P.get i
      when (not isParentExists) $ throwError err400 {errReasonPhrase = "Given \"parent\" category doesn't exist."}
      isUniqueSibling <-
        runDBDev $
          null
            <$> ( select $ do
                    c <- from $ table @Category
                    where_ (c ^. CategoryParent ==. val unEitherParentId &&. c ^. CategoryName ==. val (categoryName categ))
                    pure c
                )
      when (not isUniqueSibling) $ throwError err400 {errReasonPhrase = "Already have a category with the same \"name\" and \"parent\"."}
      runDBDev $ P.update trgId [CategoryParent P.=. unEitherParentId]
      return NoContent
    helper trgId _ (Just name) (Just parentId) = do
      let unEitherParentId = either (const Nothing) Just parentId
      isParentExists <- runDBDev $ do
        case unEitherParentId of
          Nothing -> return True
          Just i -> isJust <$> P.get i
      when (not isParentExists) $ throwError err400 {errReasonPhrase = "Given \"parent\" category doesn't exist."}
      isUniqueSibling <-
        runDBDev $
          null
            <$> ( select $ do
                    c <- from $ table @Category
                    where_ (c ^. CategoryParent ==. val unEitherParentId &&. c ^. CategoryName ==. (val name))
                    pure c
                )
      when (not isUniqueSibling) $ throwError err400 {errReasonPhrase = "Already have a category with the same \"name\" and \"parent\"."}
      runDBDev $ P.update trgId [CategoryParent P.=. unEitherParentId, CategoryName P.=. name]
      return NoContent

-- ????????????????????? WHATAFAAAAACK

getC :: Maybe Int -> Maybe Int -> Handler (WithOffset [Entity Category])
getC off lim = do
  offsetLimValid lim off
  cs <- runDBDev
    . select
    $ do
      c <- from $ table @Category
      addPagination lim off (pure c)
  return $ WithOffset (newOffset lim off) cs