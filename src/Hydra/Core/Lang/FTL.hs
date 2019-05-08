{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.Core.Lang.FTL where

import           Hydra.Prelude

import           Hydra.Core.ControlFlow.FTL as L
import           Hydra.Core.Logger.FTL      as L
import           Hydra.Core.Random.FTL      as L
import qualified Hydra.Core.State.Language  as L

class (L.ControlFlowL m, L.RandomL m, L.LoggerL m) => LangL m where
  evalStateAtomically :: L.StateL a -> m a

instance LangL m => L.StateIO m where
  atomically     = evalStateAtomically
  newVarIO       = evalStateAtomically . L.newVar
  readVarIO      = evalStateAtomically . L.readVar
  writeVarIO var = evalStateAtomically . L.writeVar var


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
