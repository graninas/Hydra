module Hydra.Core.ControlFlow.Interpreter where

import qualified Hydra.Core.ControlFlow.Language as L
import           Hydra.Prelude

import qualified Hydra.Core.Runtime              as R

interpretControlFlowF :: R.CoreRuntime -> L.ControlFlowF a -> IO a
interpretControlFlowF _ (L.Delay i next) = do
    threadDelay i
    pure $ next ()

runControlFlowL :: R.CoreRuntime -> L.ControlFlowL a -> IO a
runControlFlowL coreRt = foldFree (interpretControlFlowF coreRt)
