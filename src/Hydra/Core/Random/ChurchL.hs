{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module Hydra.Core.Random.ChurchL where

import           Hydra.Prelude

import qualified Hydra.Core.Random.Class    as L
import qualified Hydra.Core.Random.Language as L


type RandomL = F L.RandomF

instance L.Random RandomL where
  getRandomInt range = liftFC $ L.GetRandomInt range id
