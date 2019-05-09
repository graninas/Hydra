{-# LANGUAGE GADTs           #-}
{-# LANGUAGE PackageImports  #-}
{-# LANGUAGE TemplateHaskell #-}

module Hydra.Core.Random.Class where

import           Hydra.Prelude

class Random m where
    getRandomInt :: (Int, Int) -> m Int
