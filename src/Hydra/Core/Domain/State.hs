module Hydra.Core.Domain.State where

import           Hydra.Prelude

type VarId = Int

-- | Concurrent variable (STM TVar).
newtype StateVar a = StateVar
  { _varId :: VarId
  }

-- | Denotes a signaling concurrent variable.
type SignalVar = StateVar Bool
