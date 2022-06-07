module Handlers.Katip where

import Katip as K (LogContexts, LogEnv, Namespace)

data Config = Config
  { cLogEnv :: K.LogEnv,
    cLogContexts :: K.LogContexts,
    cLogNamespace :: K.Namespace
  }

newtype Handler = Handler
  { hConfig :: Config
  }
