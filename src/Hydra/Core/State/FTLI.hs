module Hydra.Core.State.FTLI where

import qualified Hydra.Core.ControlFlow.Language as L
import           Hydra.Prelude

instance L.ControlFlowL (ReaderT R.CoreRuntime IO) where
    delay = liftIO threadDelay
