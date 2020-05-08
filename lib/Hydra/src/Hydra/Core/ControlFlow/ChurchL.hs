module Hydra.Core.ControlFlow.ChurchL where

import           Hydra.Prelude

import qualified Hydra.Core.ControlFlow.Language as L
import qualified Hydra.Core.ControlFlow.Class as L

type ControlFlowL = F L.ControlFlowF


instance L.ControlFlow (F L.ControlFlowF) where
  delay i = liftFC $ L.Delay i id
