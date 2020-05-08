module Hydra.Core.CliHandlers.Interpreter where

import           Hydra.Prelude

import qualified Data.Map as Map

import qualified Hydra.Core.CliHandlers.Language as L
import qualified Hydra.Core.Lang.Language as L

type Handlers a = IORef (Map.Map String (L.LangL a))

interpretCliHandlerL :: Handlers a -> L.CliHandlerF a b -> IO b

interpretCliHandlerL handlersRef (L.Cmd cmdStr method next) = do
  modifyIORef' handlersRef (Map.insert cmdStr method)
  pure $ next ()

runCliHandlerL :: Handlers a -> L.CliHandlerL a () -> IO ()
runCliHandlerL handlersRef = foldFree (interpretCliHandlerL handlersRef)
