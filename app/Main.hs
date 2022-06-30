module Main where

import Api (app)
import qualified App.Prod as A
import Config (askPort, withConfig)
import qualified DB.Postgres as P
import Handlers.DB (migrate_)
import qualified Image.File as I
import qualified Katip.Prod as L
import qualified Network.Wai.Handler.Warp as W
import System.Directory.Internal.Prelude (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "'config_path' --migrate | 'config_path'"
    confPath : "--migrate" : _ -> withConfig confPath $ \conf ->
      P.withHandler conf migrate_
    confPath : _ -> withConfig confPath $ \conf ->
      L.parseConfig conf >>= \lConf -> L.withHandler lConf $ \logHand ->
        P.withHandler conf $ \dbHand ->
          I.withHandler conf dbHand $ \imHand ->
            A.parseConfig conf >>= \appConf -> A.withHandler logHand dbHand imHand appConf $ \appHand -> do
              p <- askPort conf
              putStrLn $ "Running on port " <> show p
              W.run p $ app appHand