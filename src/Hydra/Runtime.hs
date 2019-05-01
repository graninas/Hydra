module Hydra.Runtime
    ( module X
    , startApp
    ) where

import Hydra.Prelude

import           Hydra.Core.Runtime as X
import           Hydra.Framework.Runtime as X

import           Hydra.Framework.App.Interpreter as Impl
import           Hydra.Framework.Language as L

startApp :: X.AppRuntime -> L.AppL a -> IO a
startApp appRt app = Impl.runAppL appRt app
