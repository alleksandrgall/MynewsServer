{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Api.Pagination
  ( GetWithPagination,
    addPagination,
    newOffset,
    Offset,
    Limit,
    WithOffset (..),
  )
where

import Control.Monad (when)
import Data.Aeson (ToJSON (toJSON))
import Data.Maybe (fromMaybe)
import Database.Esqueleto.Experimental as D
import Dev
import GHC.Generics (Generic)
import Servant
import Servant.Server.Internal.ErrorFormatter (MkContextWithErrorFormatter)

type GetWithPagination (t :: [*]) a = QueryParam "limit" Limit :> QueryParam "offset" Offset :> Get t (WithOffset [a])

newtype Offset = Offset {unOffset :: Int} deriving (Show)

instance ToJSON Offset where toJSON (Offset i) = toJSON i

newtype Limit = Limit {unLimit :: Int}

instance FromHttpApiData Offset where
  parseUrlPiece s = case parseUrlPiece @Int s of
    Left e -> Left e
    Right x -> if x < 0 then Left "Offset must be greater or equal then zero." else Right . Offset $ x

instance FromHttpApiData Limit where
  parseUrlPiece s = case parseUrlPiece @Int s of
    Left e -> Left e
    Right x -> if x <= 0 then Left "Limit must be greater then zero." else Right . Limit $ x

data WithOffset a = WithOffset {offset :: Offset, content :: a}
  deriving (Generic, Show, ToJSON)

addPagination :: Maybe Limit -> Maybe Offset -> SqlQuery a -> SqlQuery a
addPagination lim off q =
  q >>= \r ->
    maybe (pure ()) (D.offset . fromIntegral . unOffset) off
      >> ( limit . fromIntegral $
             maybe
               limitDev
               (\x -> if unLimit x > limitDev then limitDev else unLimit x)
               lim
         )
      >> pure r

newOffset :: Maybe Limit -> Maybe Offset -> Offset
newOffset lim off = Offset $ maybe 0 unOffset off + maybe limitDev unLimit lim + 1
