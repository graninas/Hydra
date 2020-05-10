{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

module Hydra.Core.Lang.FTL
    ( LangL
    , evalStateAtomically
    ) where

import           Hydra.Prelude

import           Hydra.Core.Logger.FTL      as L
import           Hydra.Core.Random.FTL      as L
import qualified Hydra.Core.State.Class     as L
import qualified Hydra.Core.State.Language  as L

class (Monad m, L.RandomL m, L.LoggerL m) => LangL m where
  evalStateAtomically :: L.StateL a -> m a

instance (Monad m, LangL m) => L.StateIO m where
  newVarIO       = evalStateAtomically . L.newVar
  readVarIO      = evalStateAtomically . L.readVar
  writeVarIO var = evalStateAtomically . L.writeVar var
  retryIO        = evalStateAtomically L.retry

instance (Monad m, LangL m) => L.Atomically L.StateL m where
  atomically = evalStateAtomically


-- Doesn't work
-- class Monad m => LangL m where
--   atomically :: StateL m' => m' a -> m a
--
-- Compiles but wrong.
-- class Monad m => LangL m where
--   atomically :: StateL m => m a -> m a

-- Doesn't work
-- class (StateL sm, Monad m) => LangL m where
--   atomically :: sm a -> m a

-- Wrong, doesn't work
-- class StateLLangL m where
--   atomically :: (StateL sm, LangL m) => sm a -> m a

-- Wrong (we can't express this function without knowing the runtime.
-- atomically :: (L.StateL m1, LangL m2) => m1 a -> m2 a
-- atomically = ???
