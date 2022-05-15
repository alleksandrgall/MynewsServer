{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Api.Article (parseListToNest)
import Api.Category
import Api.User
import App (withAppConfig)
import Control.Monad.IO.Class (MonadIO (liftIO))
import DB.Queries (queryNestCategoryById)
import DB.Scheme (Category, Key (CategoryKey))
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Text (Text, pack)
import Database.Esqueleto.Experimental
import Dev
import Servant
import Servant.Multipart
import Utils

type Api =
  "user" :> UserApi
    :<|> "category" :> CategoryApi

-- type TestApi = "test_insert" :> MultipartForm Mem (MultipartData Mem) :> Put '[JSON] Text

-- testServer :: Server TestApi
-- testServer form = do
--   v <- validateImages (files form)
--   id <- liftIO $ saveInsertToDbImages v "/home/turban/metaLampServer/images" (const (return ()))
--   return $ pack . show $ id

main :: IO ()
main = undefined -- withAppConfig (\c -> runDev (Proxy :: Proxy Api)