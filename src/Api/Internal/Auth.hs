{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Api.Internal.Auth where

import App (App, runDB)
import Control.Monad (unless)
import Control.Monad.Except (throwError)
import DB.Scheme (Article (Article, articleUserId), ArticleId, User (User, userIsAdmin, userIsAuthor, userName))
import Data.Function ((&))
import Database.Persist
import Katip
import Servant

userIsAdmin_ :: Entity User -> App ()
userIsAdmin_ u =
  katipAddContext (sl "user_id" (entityKey u)) $
    katipAddNamespace "Auth admin" $
      unless (u & entityVal & userIsAdmin) $ do
        throwError err404

userAtLeastAuthor_ :: Entity User -> App ()
userAtLeastAuthor_ u =
  katipAddContext (sl "user_id" (entityKey u)) $
    katipAddNamespace "Auth author" $ do
      logFM InfoS "Checking if user at least author"
      unless ((u & entityVal & userIsAuthor) || (u & entityVal & userIsAdmin)) $ do
        throwError $ err403 {errReasonPhrase = "Author status required."}

articleBelongsToUser :: Entity User -> ArticleId -> App ()
articleBelongsToUser u aId = do
  katipAddContext (sl "user_id" (entityKey u)) $
    katipAddNamespace "Auth article owner" $ do
      logFM InfoS "Checking is user owns an article"
      maybeArt <- runDB $ get aId
      case maybeArt of
        Nothing -> throwError err400 {errReasonPhrase = "No article with id " ++ show aId}
        Just Article {..} ->
          unless ((u & entityKey) == articleUserId || (u & entityVal & userIsAdmin)) $
            throwError $ err403 {errReasonPhrase = "Provided user is not an author of that article."}
