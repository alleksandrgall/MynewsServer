{-# LANGUAGE RecordWildCards #-}

module DB.Types.Internal where

import Crypto.KDF.BCrypt (hashPassword)
import DB.Scheme
import Data.Aeson hiding (Value)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.List (find)
import qualified Data.Map as M
import Data.Maybe (isNothing)
import Data.Text (pack)
import Data.Time (UTCTime)
import Database.Esqueleto.Experimental (Entity (entityKey, entityVal), Value)
import Dev (createUser)

data NestCategory = Cat CategoryId String NestCategory | Non
  deriving (Show)

instance ToJSON NestCategory where
  toJSON Non = object []
  toJSON (Cat i name children) =
    object
      [ "id" .= i,
        "category_name" .= pack name,
        "category_sub" .= toJSON children
      ]

parseNestCat :: [Entity Category] -> NestCategory
parseNestCat ents =
  let mEnt = M.fromList $ map (\ent -> (categoryParent . entityVal $ ent, ent)) ents
      root = find (isNothing . categoryParent . entityVal) ents
      children Nothing = Non
      children (Just c) =
        Cat (entityKey c) (categoryName . entityVal $ c) $ children . M.lookup (Just $ entityKey c) $ mEnt
   in children root

data IncomingUser = IncomingUser {name :: String, password :: String, avatar :: Maybe ImageId, isAdmin_ :: Bool, isAuthor_ :: Bool}

incUserToDbUser :: IncomingUser -> IO User
incUserToDbUser IncomingUser {..} = createUser name password avatar isAdmin_ isAuthor_

instance FromJSON IncomingUser where
  parseJSON (Object o) =
    IncomingUser <$> o .: "name"
      <*> o .: "password"
      <*> o .:? "avatar"
      <*> o .:? "is_admin" .!= False
      <*> o .:? "is_author" .!= False
  parseJSON _ = mempty

data InternalUser = InternalUser
  { iUserId :: UserId,
    iUserName :: String,
    iUserAvatar :: Maybe String,
    iUserCreated :: UTCTime,
    iUserIsAdmin :: Bool,
    iUserIsAuthor :: Bool
  }

parseInternalUser :: Entity User -> InternalUser
parseInternalUser ent =
  InternalUser
    { iUserId = entityKey ent,
      iUserName = userName . entityVal $ ent,
      iUserAvatar = undefined,
      iUserCreated = userCreated . entityVal $ ent,
      iUserIsAdmin = userIsAdmin . entityVal $ ent,
      iUserIsAuthor = userIsAuthor . entityVal $ ent
    }

data InternalArticle = InternalArticle
  { iArticleId :: ArticleId,
    iArticleTitle :: String,
    iArticleUser :: InternalUser,
    iArticleCategory :: NestCategory,
    iArticleContent :: String,
    iArticleImages :: [ImageUrl]
  }

parseInternalArticle :: (Entity Article, Entity User, [Entity Category], [Value ImageId]) -> InternalArticle
parseInternalArticle (ar, u, cs, imId) =
  InternalArticle
    { iArticleId = entityKey ar,
      iArticleTitle = articleTitle . entityVal $ ar,
      iArticleUser = parseInternalUser u,
      iArticleCategory = parseNestCat cs,
      iArticleContent = articleContent . entityVal $ ar
    }

renderImageUrl :: Value ImageId -> String
renderImageUrl = show

type ImageUrl = String