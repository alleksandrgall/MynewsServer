{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Api.User
import Dev
import Servant

type Api = "user" :> UserApi

main :: IO ()
main = runDev (Proxy :: Proxy Api) userServer