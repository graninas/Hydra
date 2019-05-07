module Hydra.Core.State.FTLI where

import           Hydra.Prelude

import qualified Hydra.Core.Domain            as D
import qualified Hydra.Core.FTL               as L
import qualified Hydra.Core.RLens             as RLens
import qualified Hydra.Core.Runtime           as R

import qualified Hydra.Core.State.Interpreter as Impl
-- 
-- instance L.StateL (ReaderT R.CoreRuntime STM) where
--   newVar val = do
--     coreRt <- ask
--     r <- lift $ Impl.newVar' (coreRt ^. RLens.stateRuntime) val
--     pure $ D.StateVar r
--   readVar var = do
--     coreRt <- ask
--     lift $ Impl.readVar' (coreRt ^. RLens.stateRuntime) var
--   writeVar var val = do
--     coreRt <- ask
--     lift $ Impl.writeVar' (coreRt ^. RLens.stateRuntime) var val
--   retry    = lift retry
