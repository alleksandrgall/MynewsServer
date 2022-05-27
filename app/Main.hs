{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Api

main :: IO ()
main = app 3000 --E.withAppConfig (\config -> run 3000 . E.runApplicationM (E.logConfig config) . E.katipMiddleware InfoS . E.mkApplication' (Proxy @Api) app $ config)

-- runDBDev $ runMigration migrateAll
-- withAppConfig (\config -> )run 3000 . katipMiddleware (logConfig config) InfoS . serveApp (Proxy :: Proxy Api) app $ config