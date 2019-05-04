{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module Hydra.Core.Process.FTL where

import           Hydra.Prelude

import qualified Hydra.Core.Domain as D


-- class (Monad m') => ProcessL m' where
--   forkProcess  :: m' a -> ProcessL m' (D.ProcessPtr a)
--   killProcess  :: D.ProcessPtr a -> ProcessL m' ()
--   tryGetResult :: D.ProcessPtr a -> ProcessL m' (Maybe a)
--   awaitResult  :: D.ProcessPtr a -> ProcessL m' a
