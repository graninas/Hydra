{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module Hydra.Core.Process.Class where

import           Hydra.Prelude

import qualified Hydra.Core.Domain               as D

class (Monad m) => Process lang m | m -> lang where
  forkProcess  :: lang a -> m (D.ProcessPtr a)
  killProcess  :: D.ProcessPtr a -> m ()
  tryGetResult :: D.ProcessPtr a -> m (Maybe a)
  awaitResult  :: D.ProcessPtr a -> m a
