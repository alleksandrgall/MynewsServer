{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (runStderrLoggingT)
import DB.Queries
import DB.Scheme (Category (Category), Unique (UniqueCatName))
import Database.Esqueleto.Experimental
import Database.Persist.Postgresql

connectionString = "host=localhost port=5432 user=turban dbname=news_db password=1781"

main :: IO ()
main = do
  runStderrLoggingT $
    withPostgresqlPool connectionString 1 $ \pool -> liftIO $ do
      flip runSqlPersistMPool pool $ do
        cats <- getNestCategoryByName "TH"
        liftIO $ print cats