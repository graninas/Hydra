{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hydra.Core.State.FTL where

import           Hydra.Prelude
import Control.Concurrent.STM as STM

-- import qualified Hydra.Core.Domain as D

class Monad m => StateL m where
  type StateVar m :: * -> *
  newVar   :: a -> m (StateVar m a)
  readVar  :: StateVar m a -> m a
  writeVar :: StateVar m a -> a -> m ()
  retry    :: m a

-- -- | Modify variable with function.
modifyVar :: StateL m => StateVar m a -> (a -> a) -> m ()
modifyVar var f = readVar var >>= writeVar var . f
{-# SPECIALIZE modifyVar :: TVar a -> (a -> a) -> STM () #-}

instance StateL STM where
  type StateVar STM = TVar
  newVar = newTVar
  {-# INLINE newVar #-}
  readVar = readTVar
  {-# INLINE readVar #-}
  writeVar = writeTVar
  {-# INLINE writeVar #-}
  retry = STM.retry
  {-# INLINE retry #-}

-- | Class that defines how can we run internal nested transaction in the
-- current computation.
class Atomic m where
  type Transaction m :: * -> *
  transaction :: (Transaction m) a -> m a

-- -- -- | Eval "delayed" logger: it will be written after successfull state operation.
-- -- evalStmLogger :: L.LoggerL () -> StateL ()
-- -- evalStmLogger action = liftF $ EvalStmLogger action id
--
-- -- instance L.Logger StateL where
-- --    logMessage level = evalStmLogger . L.logMessage level
