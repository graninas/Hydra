module Hydra.Runtime
    ( module X
    , startApp
    ) where

import Hydra.Prelude

import           Hydra.Core.Runtime as X
import           Hydra.Framework.Runtime as X

import qualified Hydra.Framework.App.Interpreter as Impl
import qualified Hydra.Framework.Language as L
import qualified Hydra.Framework.FTL as FTL

startApp :: X.AppRuntime -> L.AppL a -> IO a
startApp appRt app = Impl.runAppL appRt app
