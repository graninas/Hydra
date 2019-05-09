{-# LANGUAGE GADTs           #-}
{-# LANGUAGE PackageImports  #-}
{-# LANGUAGE TemplateHaskell #-}

module Hydra.Core.Random.ChurchL where

import           Hydra.Prelude

import qualified  Hydra.Core.Random.Language as L
import qualified  Hydra.Core.Random.Class    as L

import           Language.Haskell.TH.MakeFunctor

type RandomL = F L.RandomF

instance L.Random (F L.RandomF) where
    getRandomInt range = liftFC $ L.GetRandomInt range id
