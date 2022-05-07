module App where

import Servant (Handler)

data Config

data App a = ReaderT Config (Handler a)