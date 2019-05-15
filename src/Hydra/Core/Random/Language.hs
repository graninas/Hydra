{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module Hydra.Core.Random.Language where

import           Hydra.Prelude

import           Hydra.Core.Random.Class

import           Language.Haskell.TH.MakeFunctor

-- | Language for Random.
data RandomF next where
    -- | Get Int from range
    GetRandomInt :: (Int, Int) -> (Int -> next) -> RandomF next

makeFunctorInstance ''RandomF

type RandomL = Free RandomF

instance Random RandomL where
  getRandomInt range = liftF $ GetRandomInt range id
