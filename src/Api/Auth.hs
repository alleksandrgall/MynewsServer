{-# LANGUAGE RecordWildCards #-}

module Api.Auth where

import App (App)
import Control.Monad (unless)
import Control.Monad.Except (throwError)
import DB.Scheme (User (User, userIsAdmin, userIsAuthor, userName))
import Data.Function ((&))
import Database.Persist
import Katip (katipAddContext, katipAddNamespace, sl)
import Servant

userIsAdmin_ :: Entity User -> App ()
userIsAdmin_ u =
  katipAddContext (sl "user_id" (entityKey u)) $
    katipAddNamespace "Auth admin" $
      unless (u & entityVal & userIsAdmin) $ do
        throwError err402

userIsAuthor_ :: Entity User -> App ()
userIsAuthor_ u =
  katipAddContext (sl "user_id" (entityKey u)) $
    katipAddNamespace "Auth author" $
      unless (u & entityVal & userIsAuthor) $ do
        throwError $ err403 {errReasonPhrase = "Author status required."}
