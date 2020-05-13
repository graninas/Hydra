module Hydra.Framework.Runtime where

import           Hydra.Prelude

import qualified Hydra.Core.Domain  as D
import qualified Hydra.Core.Runtime as R

-- | App runtime data.
data AppRuntime = AppRuntime
    { _coreRuntime    :: R.CoreRuntime
    }

createAppRuntime' :: R.LoggerRuntime -> IO AppRuntime
createAppRuntime' loggerRt = AppRuntime
    <$> R.createCoreRuntime loggerRt

createAppRuntime :: R.CoreRuntime -> IO AppRuntime
createAppRuntime = pure . AppRuntime

clearAppRuntime :: AppRuntime -> IO ()
clearAppRuntime _ = pure ()

withAppRuntime :: Maybe D.LoggerConfig -> (AppRuntime -> IO a) -> IO a
withAppRuntime mbLoggerCfg appF =
  bracket createLogger' R.clearLoggerRuntime $ \loggerRt ->
  bracket (R.createCoreRuntime loggerRt) R.clearCoreRuntime $ \coreRt ->
  bracket (createAppRuntime coreRt) clearAppRuntime appF
  where
    createLogger' = case mbLoggerCfg of
      Nothing        -> R.createVoidLoggerRuntime
      Just loggerCfg -> R.createLoggerRuntime loggerCfg
