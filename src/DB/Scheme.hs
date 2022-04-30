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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module DB.Scheme where

import Data.Aeson
import qualified Data.ByteString as BS
import Data.Time
import Database.Esqueleto.Experimental
import Database.Persist.TH
import GHC.Exts (fromList)
import GHC.Generics (Generic)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
User
    Id          sql=user_id
    name String sql=user_name 
    UniqueUserName name
    avatar ImageId Maybe
    passwordHash BS.ByteString
    created UTCTime default=CURRENT_DATE
    isAdmin Bool
    isAuthor Bool
Category
    Id          sql=category_id
    name String sql=category_name
    parent CategoryId Maybe sql=parent_category
    deriving Generic Show
Article
    Id          sql=article_id
    title String sqltype=varchar(255)
    userId UserId
    categoryId CategoryId
    content String
    isPublished Bool
Image
    Id          sql=image_id
    mime String
    path String
ImageArticle
    Id sql=image_article_id
    articleId ArticleId
    imageId ImageId
|]

instance ToJSON User where
  toJSON User {..} =
    object
      [ "username" .= userName,
        "avatar" .= userAvatar,
        "created" .= userCreated,
        "isAdmin" .= userIsAdmin,
        "isAuthor" .= userIsAuthor
      ]

instance ToJSON (Entity User) where
  toJSON = entityIdToJSON

instance ToJSON Category where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

instance ToJSON (Entity Category) where
  toJSON = entityIdToJSON