{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Api.Internal.Pagination
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
import Database.Esqueleto.Internal.Internal (SqlSelect)
import GHC.Generics (Generic)
import Servant
import Servant.API.Modifiers (RequiredArgument)
import Servant.Server.Internal.ErrorFormatter (MkContextWithErrorFormatter)

type GetWithPagination (t :: [*]) a = QueryParam "limit" Limit :> QueryParam "offset" Offset :> Get t (WithOffset a)

newtype Offset = Offset {unOffset :: Int} deriving (Show)

instance ToJSON Offset where toJSON (Offset i) = toJSON i

newtype Limit = Limit {unLimit :: Int}

instance ToJSON Limit where toJSON (Limit i) = toJSON i

instance FromHttpApiData Offset where
  parseUrlPiece s = case parseUrlPiece @Int s of
    Left e -> Left e
    Right x -> if x < 0 then Left "Offset must be an inteher greater or equal then zero." else Right . Offset $ x

instance FromHttpApiData Limit where
  parseUrlPiece s = case parseUrlPiece @Int s of
    Left e -> Left e
    Right x -> if x <= 0 then Left "Limit must be an integer greater then zero." else Right . Limit $ x

data WithOffset a = WithOffset {offset :: Offset, content :: [a]}
  deriving (Functor, Generic, Show, ToJSON)

instance Foldable WithOffset where
  foldMap f (WithOffset off list) = foldMap f list

instance Traversable WithOffset where
  traverse f (WithOffset off list) = WithOffset off <$> traverse f list

setLimit :: Int -> Maybe Limit -> SqlQuery ()
setLimit maxLimit lim =
  E.limit . fromIntegral $
    maybe
      maxLimit
      (\x -> if unLimit x > maxLimit then maxLimit else unLimit x)
      lim

setOffset :: Maybe Offset -> SqlQuery ()
setOffset = maybe (pure ()) (E.offset . fromIntegral . unOffset)

selectPagination ::
  (MonadIO m, SqlSelect a r) =>
  Maybe Limit ->
  Maybe Offset ->
  Int ->
  SqlQuery a ->
  SqlPersistT m (WithOffset r)
selectPagination lim off maxLimit q =
  WithOffset (newOffset lim off maxLimit)
    <$> ( select $ do
            r <- q
            setOffset off
            setLimit maxLimit lim
            pure r
        )

newOffset :: Maybe Limit -> Maybe Offset -> Int -> Offset
newOffset lim off maxLimit = Offset $ maybe 0 unOffset off + maybe maxLimit unLimit lim + 1
