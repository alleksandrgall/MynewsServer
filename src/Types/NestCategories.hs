module Types.NestCategories where

import DB.Scheme (Category (..))
import Data.Aeson
import Data.Function ((&))
import Data.Text (pack)

data NestCategory = Cat String NestCategory | Non
  deriving (Show)

instance ToJSON NestCategory where
  toJSON Non = object []
  toJSON (Cat name children) =
    object
      [ "category_name" .= pack name,
        "category_sub" .= toJSON children
      ]