{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api.Pagination where

import Control.Monad (when)
import Data.Aeson (ToJSON)
import Data.Maybe (fromMaybe)
import Database.Esqueleto.Experimental as D
import Dev
import GHC.Generics (Generic)
import Servant

data WithOffset a = WithOffset {offset :: Int, content :: a}
  deriving (Generic, Show, ToJSON)

addPagination :: Maybe Int -> Maybe Int -> SqlQuery a -> SqlQuery a
addPagination lim off q =
  q >>= \r ->
    maybe (pure ()) (D.offset . fromIntegral) off
      >> (limit . fromIntegral $ fromMaybe limitDev lim)
      >> pure r

newOffset :: Maybe Int -> Maybe Int -> Int
newOffset lim off = fromMaybe 0 off + fromMaybe limitDev lim + 1

offsetLimValid :: Maybe Int -> Maybe Int -> Handler ()
offsetLimValid lim off =
  when (maybe False (<= 0) lim || maybe False (< 0) off) (throwError err400 {errReasonPhrase = "Offset and limit must be positive and non negative responsibly."})