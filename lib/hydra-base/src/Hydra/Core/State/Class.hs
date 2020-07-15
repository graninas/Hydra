{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module Hydra.Core.State.Class where

import           Hydra.Prelude

import qualified Hydra.Core.Domain as D

class Monad m => State' m where
  newVar   :: a -> m (D.StateVar a)
  readVar  :: D.StateVar a -> m a
  writeVar :: D.StateVar a -> a -> m ()
  retry    :: m a

class Monad m => StateIO m where
  newVarIO   :: a -> m (D.StateVar a)
  readVarIO  :: D.StateVar a -> m a
  writeVarIO :: D.StateVar a -> a -> m ()
  retryIO    :: m a

class (Monad m, State' s) => Atomically s m | m -> s where
  atomically :: s a -> m a

-- | Modify variable with function.
modifyVar :: State' m => D.StateVar a -> (a -> a) -> m ()
modifyVar var f = readVar var >>= writeVar var . f
