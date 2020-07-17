module Hydra.Core.ControlFlow.Language where

import           Hydra.Prelude

import qualified Hydra.Core.ControlFlow.Class as C

data ControlFlowF next where
  -- | Freeze the current thread on time (in microseconds).
  Delay :: Int -> (() -> next) -> ControlFlowF next

instance Functor ControlFlowF where
    fmap g (Delay i next) = Delay i (g . next)

type ControlFlowL = F ControlFlowF

instance C.ControlFlow (F ControlFlowF) where
    delay i = liftFC $ Delay i id
