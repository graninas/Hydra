module Hydra.Core.State.FTL where

import           Hydra.Prelude

import qualified Hydra.Core.Domain as D

-- class Monad m => StateL m where
--   newVar   :: a -> m (D.StateVar a)
--   readVar  :: D.StateVar a -> m a
--   writeVar :: D.StateVar a -> a -> m ()
--   retry    :: m a

-- -- | Modify variable with function.
-- modifyVar :: StateL m => D.StateVar a -> (a -> a) -> m ()
-- modifyVar var f = readVar var >>= writeVar var . f


-- class StateIO m where
--   atomically :: StateL a -> m a
--   newVarIO :: a -> m (D.StateVar a)
--   readVarIO :: D.StateVar a -> m a
--   writeVarIO :: D.StateVar a -> a -> m ()
--

-- -- -- | Eval "delayed" logger: it will be written after successfull state operation.
-- -- evalStmLogger :: L.LoggerL () -> StateL ()
-- -- evalStmLogger action = liftF $ EvalStmLogger action id
--
-- -- instance L.Logger StateL where
-- --     logMessage level = evalStmLogger . L.logMessage level
