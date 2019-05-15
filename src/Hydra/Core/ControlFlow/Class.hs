module Hydra.Core.ControlFlow.Class where

import           Hydra.Prelude

class Monad m => ControlFlow m where
    delay :: Int -> m ()
