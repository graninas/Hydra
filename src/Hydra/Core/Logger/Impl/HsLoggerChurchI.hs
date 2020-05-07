module Hydra.Core.Logger.Impl.HsLoggerChurchI where

import           Hydra.Prelude

import qualified Hydra.Core.Logger.Impl.HsLoggerInterpreter as I

runLoggerL (Just h) l = foldF (I.interpretLoggerF h) l
runLoggerL Nothing  _ = pure ()
