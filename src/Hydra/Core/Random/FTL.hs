{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module Hydra.Core.Random.FTL where

import           Hydra.Prelude

import qualified Hydra.Core.Domain as D


-- class Monad m => RandomL m where
--   getRandomInt :: (Int, Int) -> m Int
