module Hydra.Core.ControlFlow.ChurchI where

import qualified Hydra.Core.ControlFlow.ChurchL     as CL
import qualified Hydra.Core.ControlFlow.Interpreter as I
import qualified Hydra.Core.ControlFlow.Language    as L
import           Hydra.Prelude

import qualified Hydra.Core.Runtime                 as R

runControlFlowL :: R.CoreRuntime -> CL.ControlFlowL a -> IO a
runControlFlowL coreRt = foldF (I.interpretControlFlowF coreRt)
