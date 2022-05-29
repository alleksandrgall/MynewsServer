{-# LANGUAGE RecordWildCards #-}

module SqlException where

import Control.Exception (Exception)
import Control.Exception.Base (Exception (fromException))
import Database.PostgreSQL.Simple (SqlError (SqlError, sqlState))

data SqlException
  = NotExists
  | AlreadyExists
  deriving (Show)

instance Exception SqlException where
  fromException e
    | Just SqlError {..} <- fromException e = case sqlState of
      "23503" -> Just NotExists
      "23505" -> Just AlreadyExists
      _ -> Nothing
    | otherwise = Nothing