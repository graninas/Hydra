module Hydra.Core.ControlFlow.Class where

import           Hydra.Prelude

class ControlFlow m where
    delay :: Int -> m ()
