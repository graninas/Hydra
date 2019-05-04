{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module Hydra.Core.Lang.Language where

import           Hydra.Prelude

import           Hydra.Core.ControlFlow.Language as L
import           Hydra.Core.Logger.Language      as L
import           Hydra.Core.Random.Language      as L
import           Hydra.Core.State.Language       as L





class Monad m => IOL m where
  evalIO :: IO a -> m a

class (Monad m, IOL m) => LangL m where
  evalStateAtomically :: L.StateL a -> m a
  evalLogger :: L.LoggerL () -> m ()
  evalRandom :: L.RandomL a -> m a
  evalControlFlow :: L.ControlFlow a -> m a
