{-# LANGUAGE RecordWildCards #-}

module Api.Internal.Auth where

import Control.Monad (unless)
import Data.Function ((&))
import Database.Persist
  ( Entity (entityKey, entityVal),
    PersistStoreRead (get),
  )
import Handlers.App (App, runDB)
import Handlers.DB.Scheme (Article (Article, articleUserId), ArticleId, User (userIsAdmin, userIsAuthor))
import qualified Katip as K
import Servant
  ( ServerError (errReasonPhrase),
    err400,
    err403,
    err404,
    throwError,
  )

userIsAdmin_ :: Entity User -> App imageM ()
userIsAdmin_ u =
  K.katipAddContext (K.sl "user_id" (entityKey u)) $
    K.katipAddNamespace "Auth admin" $ do
      unless (u & entityVal & userIsAdmin) $ throwError err404
      K.logFM K.InfoS "Auth success"

userAtLeastAuthor_ :: Entity User -> App imageM ()
userAtLeastAuthor_ u =
  K.katipAddContext (K.sl "user_id" (entityKey u)) $
    K.katipAddNamespace "Auth author" $ do
      unless ((u & entityVal & userIsAuthor) || (u & entityVal & userIsAdmin)) $
        throwError $ err403 {errReasonPhrase = "Author status required."}
      K.logFM K.InfoS "Auth success"

articleBelongsToUser :: Entity User -> ArticleId -> App imageM ()
articleBelongsToUser u aId = do
  K.katipAddContext (K.sl "user_id" (entityKey u)) $
    K.katipAddNamespace "Auth article owner" $ do
      maybeArt <- runDB $ get aId
      case maybeArt of
        Nothing -> throwError err400 {errReasonPhrase = "No article with id " ++ show aId}
        Just Article {..} -> do
          unless ((u & entityKey) == articleUserId || (u & entityVal & userIsAdmin)) $
            throwError $ err403 {errReasonPhrase = "Provided user is not an author of that article."}
          K.logFM K.InfoS "Auth success"
