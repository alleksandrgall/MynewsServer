module Main where

import Api
import qualified App.Prod as A
import Config
import qualified DB.Postgres as P
import qualified Handlers.App as A
import qualified Katip.Prod as L

configDefault :: A.ConfigDefault
configDefault =
  A.ConfigDefault
    { A.defMaxImageSize = 20000000,
      A.defCPaginationLimit = 5,
      A.defCMaxImageUpload = 5
    }

main :: IO ()
main = withConfig "/home/turban/metaLampServer/config/configDev.cfg" $ \conf ->
  L.parseConfig conf >>= \lConf -> L.withHandler lConf $ \logHand ->
    P.withHandler conf $ \dbHand ->
      A.parseConfig configDefault conf >>= \appConf -> A.withHandler logHand dbHand appConf $ \appHand ->
        app appHand 3000

-- runDBDev $ runMigration migrateAll
-- withAppConfig (\config -> )run 3000 . katipMiddleware (logConfig config) InfoS . serveApp (Proxy :: Proxy Api) app $ config