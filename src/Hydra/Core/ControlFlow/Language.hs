module Hydra.Core.ControlFlow.Language where

import           Hydra.Prelude

data ControlFlowF next where
  -- | Freeze the current thread on time (in microseconds).
  Delay :: Int -> (() -> next) -> ControlFlowF next

instance Functor ControlFlowF where
    fmap g (Delay i next) = Delay i (g . next)

type ControlFlowL next = Free ControlFlowF next

class ControlFlow m where
    delay :: Int -> m ()

instance ControlFlow (Free ControlFlowF) where
    delay i = liftF $ Delay i id
