{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module Hydra.Core.Random.FTL where

import           Hydra.Prelude



class Monad m => RandomL m where
  getRandomInt :: (Int, Int) -> m Int
