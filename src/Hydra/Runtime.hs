module Hydra.Runtime
    ( module X
    , startApp
    ) where

import           Hydra.Prelude

import           Hydra.Core.Runtime              as X
import           Hydra.Framework.Runtime         as X

import qualified Hydra.Framework.App.Interpreter as Impl
import qualified Hydra.Framework.FTL             as FTL
import qualified Hydra.Framework.Language        as L

startApp :: X.CoreRuntime -> L.AppL a -> IO a
startApp coreRt app = Impl.runAppL coreRt app
