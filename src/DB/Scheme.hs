{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
--{-# LANGUAGE EmptyDataDecls #-}
--{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module DB.Scheme where

import Data.Aeson
import Data.ByteString
import Data.Time
import Database.Esqueleto.Experimental
import Database.Persist.TH
import GHC.Generics (Generic)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
User
    Id          sql=user_id
    name String sql=user_name 
    UniqueUserName name
    avatar ByteString
    passwordHash ByteString
    created UTCTime default=CURRENT_DATE
    isAdmin Bool
    isAuthor Bool
Category
    Id          sql=category_id
    name String sql=category_name
    UniqueCatName name
    parent CategoryId Maybe sql=parent_category
    deriving Generic Show
Article
    Id          sql=article_id
    description String
    userId UserId
    categoryId CategoryId
    content String
Image
    Id          sql=image_id
    bytes ByteString sql=image_bytes
    articleId ArticleId
|]
