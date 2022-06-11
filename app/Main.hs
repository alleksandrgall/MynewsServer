module Main where

import Api (app)
import qualified App.Prod as A
import Config (withConfig)
import qualified DB.Postgres as P
import qualified Katip.Prod as L
import qualified Network.Wai.Handler.Warp as W

main :: IO ()
main = withConfig "/home/turban/metaLampServer/config/configDev.cfg" $ \conf ->
  L.parseConfig conf >>= \lConf -> L.withHandler lConf $ \logHand ->
    P.withHandler conf $ \dbHand ->
      A.parseConfig conf >>= \appConf -> A.withHandler logHand dbHand appConf $ \appHand ->
        W.run 3000 $ app appHand