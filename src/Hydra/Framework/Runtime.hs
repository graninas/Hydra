module Hydra.Framework.Runtime where

import           Hydra.Prelude

import qualified Data.Map                        as Map

import qualified Hydra.Core.Domain               as D
import qualified Hydra.Core.Runtime              as R

-- | App runtime data.
data AppRuntime = AppRuntime
    { _coreRuntime :: R.CoreRuntime
    , _processRuntime :: R.ProcessRuntime
    }

createAppRuntime :: R.LoggerRuntime -> IO AppRuntime
createAppRuntime loggerRt = AppRuntime
    <$> R.createCoreRuntime loggerRt
    <*> R.createProcessRuntime
