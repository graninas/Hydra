module Hydra.Core.ControlFlow.FTLI where

import           Hydra.Prelude

class Monad m => ControlFlowL m where
    delay :: Int -> m ()
