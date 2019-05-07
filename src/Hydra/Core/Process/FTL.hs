{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Hydra.Core.Process.FTL where

import           Hydra.Prelude

import qualified Hydra.Core.Domain    as D
import           Hydra.Core.Evaluable


class (Monad m) => ProcessL m where
  forkProcess  :: Evaluable m' => m' a -> m (D.ProcessPtr a)
  -- killProcess  :: D.ProcessPtr a -> m ()
  -- tryGetResult :: D.ProcessPtr a -> m (Maybe a)
  -- awaitResult  :: D.ProcessPtr a -> m a
