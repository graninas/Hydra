module Hydra.Core.Random.FTLI where

import           Hydra.Prelude
import           System.Entropy
import           System.Random                hiding (next)

import qualified Hydra.Core.FTL               as L
import qualified Hydra.Core.RLens             as RLens
import qualified Hydra.Core.Runtime           as R
import qualified Hydra.Core.State.Interpreter as Impl
import qualified Hydra.Core.State.Language    as L

instance L.RandomL (ReaderT R.CoreRuntime IO) where
  getRandomInt range = liftIO $ randomRIO range
