module Hydra.Core.ControlFlow.FTL where

import           Hydra.Prelude

class Monad m => ControlFlowL m where
  delay :: Int -> m ()
