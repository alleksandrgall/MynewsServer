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
    selectPagination,
    Offset,
    Limit,
    WithOffset,
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (ToJSON (toJSON))
import Data.Data (Typeable)
import Data.Maybe (fromMaybe)
import Database.Esqueleto.Experimental as E
import Dev
import GHC.Generics (Generic)
import Servant
import Servant.API.Modifiers (RequiredArgument)
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

selectPagination ::
  (MonadIO m, PersistEntity a) =>
  Maybe Limit ->
  Maybe Offset ->
  SqlQuery (SqlExpr (Entity a)) ->
  SqlPersistT m (WithOffset [Entity a])
selectPagination lim off q =
  WithOffset (newOffset lim off)
    <$> ( select $ do
            r <- q
            maybe (pure ()) (E.offset . fromIntegral . unOffset) off
            E.limit . fromIntegral $
              maybe
                limitDev
                (\x -> if unLimit x > limitDev then limitDev else unLimit x)
                lim
            pure r
        )

newOffset :: Maybe Limit -> Maybe Offset -> Offset
newOffset lim off = Offset $ maybe 0 unOffset off + maybe limitDev unLimit lim + 1
