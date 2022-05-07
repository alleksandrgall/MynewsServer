{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Api.Article (parseListToNest)
import Api.Category
import Api.User
import DB.Queries (queryNestCategoryById)
import DB.Scheme (Category, Key (CategoryKey))
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BS
import Database.Esqueleto.Experimental
import Dev
import Servant

type Api =
  "user" :> UserApi
    :<|> "category" :> CategoryApi

main :: IO ()
main = runDev (Proxy :: Proxy Api) (userServer :<|> categoryServer)