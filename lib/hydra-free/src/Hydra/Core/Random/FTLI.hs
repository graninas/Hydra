module Hydra.Core.Random.FTLI where

import           Hydra.Prelude
import           System.Random                hiding (next)

import qualified Hydra.Core.FTL               as L
import qualified Hydra.Runtime                as R

instance MonadIO m => L.RandomL (ReaderT R.CoreRuntime m) where
  getRandomInt range = liftIO $ randomRIO range
