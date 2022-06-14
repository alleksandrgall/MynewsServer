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

module Handlers.DB.Scheme where

import Data.Aeson
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    Options (fieldLabelModifier),
    ToJSON (toJSON),
    camelTo2,
    defaultOptions,
    genericParseJSON,
    genericToJSON,
    object,
  )
import qualified Data.ByteString as BS
import Data.Time (Day)
import Database.Persist
  ( Entity,
    FieldDef
      ( fieldAttrs,
        fieldCascade,
        fieldComments,
        fieldDB,
        fieldGenerated,
        fieldHaskell,
        fieldIsImplicitIdColumn,
        fieldReference,
        fieldSqlType,
        fieldStrict,
        fieldType
      ),
    entityIdFromJSON,
    entityIdToJSON,
  )
import Database.Persist.TH
  ( mkMigrate,
    mkPersist,
    persistLowerCase,
    share,
    sqlSettings,
  )
import GHC.Generics (Generic)

{-
CATEGORY !!!!
Can't add uniqueness constraint to Category table based on the restriction that two categories with the same parent can't have the same name.
Reason: By default we disallow NULLables in an uniqueness constraint.  The semantics of how NULL interacts with those constraints is non-trivial:
  two NULL values are not considered equal for the purposes of an uniqueness constraint.
Could be worked around with adding "root category", decided not to, since it complicates migrations and altering method.
Hence it upper mentioned criteria should be checked manualy in `Api.Category.alter`.
-}

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
User
    Id          sql=user_id
    name String sql=user_name 
    UniqueUserName name
    avatar ImageId Maybe
    passwordHash BS.ByteString
    created Day default=CURRENT_DATE
    isAdmin Bool
    isAuthor Bool
Category
    Id          sql=category_id
    name String sql=category_name
    parent CategoryId Maybe sql=parent_category OnDeleteCascade
    deriving Generic Show
Article
    Id          sql=article_id
    title String sqltype=varchar(255)
    created Day default=CURRENT_DATE
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
    articleId ArticleId OnDeleteCascade
    imageId ImageId OnDeleteCascade
|]

deriving instance Show Image

instance Eq Category where
  (Category name1 par1) == (Category name2 par2) = (name1 == name2) && (par1 == par2)

instance ToJSON Category where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

instance FromJSON Category where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

instance ToJSON (Entity Category) where
  toJSON = entityIdToJSON

instance FromJSON (Entity Category) where
  parseJSON = entityIdFromJSON