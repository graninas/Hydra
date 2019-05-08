{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Hydra.Core.Process.FTL where

import           Hydra.Prelude

import qualified Hydra.Core.Domain    as D



-- class (Monad m) => ProcessL m where
--   forkProcess  :: m' a -> m (D.ProcessPtr a)
  -- killProcess  :: D.ProcessPtr a -> m ()
  -- tryGetResult :: D.ProcessPtr a -> m (Maybe a)
  -- awaitResult  :: D.ProcessPtr a -> m a
