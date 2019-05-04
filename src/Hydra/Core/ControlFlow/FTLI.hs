module Hydra.Core.ControlFlow.FTLI where

import           Hydra.Prelude

import qualified Hydra.Core.FTL     as L
import qualified Hydra.Core.RLens   as RLens
import qualified Hydra.Core.Runtime as R

instance L.ControlFlowL (ReaderT R.CoreRuntime IO) where
    delay = liftIO . threadDelay
