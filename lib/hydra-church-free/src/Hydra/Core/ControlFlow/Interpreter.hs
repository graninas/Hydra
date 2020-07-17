module Hydra.Core.ControlFlow.Interpreter where

import           Hydra.Prelude

import qualified Hydra.Core.ControlFlow.Language as L
import qualified Hydra.Runtime as R

interpretControlFlowF :: R.CoreRuntime -> L.ControlFlowF a -> IO a
interpretControlFlowF _ (L.Delay i next) = do
    threadDelay i
    pure $ next ()

runControlFlowL :: R.CoreRuntime -> L.ControlFlowL a -> IO a
runControlFlowL coreRt = foldF (interpretControlFlowF coreRt)
