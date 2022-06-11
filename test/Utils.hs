{-# LANGUAGE DeriveAnyClass #-}

module Utils where

import Control.Exception (Exception, throwIO)
import Test.Hspec (expectationFailure)

shouldBeRightOr :: (Exception e) => String -> Either e a -> IO a
shouldBeRightOr s (Left e) = expectationFailure (s <> ": " <> show e) >> throwIO e
shouldBeRightOr s (Right u) = return u

data JustExpected = JustExpected deriving (Show, Exception)

shouldBeJustOr :: String -> Maybe a -> IO a
shouldBeJustOr s Nothing = expectationFailure s >> throwIO JustExpected
shouldBeJustOr s (Just x) = return x
