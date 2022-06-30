{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Api.Internal.Pagination
  ( GetWithPagination,
    selectPagination,
    Offset (..),
    Limit (..),
    WithOffset (content),
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON, ToJSON (toJSON), Value (Number))
import Data.Aeson.Types (FromJSON (parseJSON))
import qualified Database.Esqueleto.Experimental as E
import Database.Esqueleto.Internal.Internal (SqlSelect)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Servant (FromHttpApiData (parseUrlPiece), Get, QueryParam, ToHttpApiData (toUrlPiece), type (:>))

type GetWithPagination (t :: [*]) a = QueryParam "limit" Limit :> QueryParam "offset" Offset :> Get t (WithOffset a)

newtype Offset = Offset {unOffset :: Int} deriving (Show)

instance ToJSON Offset where toJSON (Offset i) = toJSON i

instance FromJSON Offset where
  parseJSON (Number n) = Offset <$> parseJSON (Number n)
  parseJSON _ = mempty

newtype Limit = Limit {unLimit :: Natural}

instance ToJSON Limit where toJSON (Limit i) = toJSON i

instance FromHttpApiData Offset where
  parseUrlPiece s = case parseUrlPiece @Int s of
    Left e -> Left e
    Right x -> if x < 0 then Left "Offset must be an inteher greater or equal then zero." else Right . Offset $ x

instance ToHttpApiData Limit where
  toUrlPiece (Limit x) = toUrlPiece x

instance ToHttpApiData Offset where
  toUrlPiece (Offset x) = toUrlPiece x

instance FromHttpApiData Limit where
  parseUrlPiece s = case parseUrlPiece @Natural s of
    Left e -> Left e
    Right x -> Right . Limit $ x

data WithOffset a = WithOffset {offset :: Offset, content :: [a]}
  deriving (Functor, Generic, Show, ToJSON, FromJSON)

instance Foldable WithOffset where
  foldMap f (WithOffset _ list) = foldMap f list

instance Traversable WithOffset where
  traverse f (WithOffset off list) = WithOffset off <$> traverse f list

setLimit :: Natural -> Maybe Limit -> E.SqlQuery ()
setLimit maxLimit lim =
  E.limit . fromIntegral $
    maybe
      maxLimit
      (\x -> if unLimit x > maxLimit then maxLimit else unLimit x)
      lim

setOffset :: Maybe Offset -> E.SqlQuery ()
setOffset = maybe (pure ()) (E.offset . fromIntegral . unOffset)

selectPagination ::
  (MonadIO m, SqlSelect a r) =>
  Maybe Limit ->
  Maybe Offset ->
  Natural ->
  E.SqlQuery a ->
  E.SqlPersistT m (WithOffset r)
selectPagination lim off maxLimit q =
  WithOffset (newOffset lim off maxLimit)
    <$> ( E.select $ do
            r <- q
            setOffset off
            setLimit maxLimit lim
            pure r
        )

newOffset :: Maybe Limit -> Maybe Offset -> Natural -> Offset
newOffset lim off maxLimit = Offset $ maybe 0 unOffset off + fromIntegral (maybe maxLimit unLimit lim) + 1
