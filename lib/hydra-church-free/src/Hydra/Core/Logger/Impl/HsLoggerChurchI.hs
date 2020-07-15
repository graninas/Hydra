module Hydra.Core.Logger.Impl.HsLoggerChurchI where

import           Hydra.Prelude

import           Hydra.Core.Logger.Impl.HsLogger (HsLoggerHandle)
import           Hydra.Core.Logger.Language (LoggerF)
import qualified Hydra.Core.Logger.Impl.HsLoggerInterpreter as I



runLoggerL :: Maybe HsLoggerHandle -> F LoggerF () -> IO ()
runLoggerL (Just h) l = foldF (I.interpretLoggerF h) l
runLoggerL Nothing  _ = pure ()
