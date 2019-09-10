module Hydra.Runtime
    ( module X
    , startApp
    ) where

import           Hydra.Prelude

import           Hydra.Core.Runtime              as X
import           Hydra.Framework.Runtime         as X

import qualified Hydra.Framework.App.ChurchI     as CI
import qualified Hydra.Framework.App.Interpreter as I
import qualified Hydra.Framework.ChurchL         as CL
import qualified Hydra.Framework.Language        as L

class StartApp m where
  startApp :: X.AppRuntime -> m a -> IO a

instance StartApp L.AppL where
  startApp = I.runAppL

instance StartApp CL.AppL where
  startApp = CI.runAppL
