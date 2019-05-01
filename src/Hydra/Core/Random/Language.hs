{-# LANGUAGE GADTs           #-}
{-# LANGUAGE PackageImports  #-}
{-# LANGUAGE TemplateHaskell #-}

module Hydra.Core.Random.Language where

import           Hydra.Prelude

import           Language.Haskell.TH.MakeFunctor

-- | Language for Random.
data RandomF next where
    -- | Get Int from range
    GetRandomInt :: (Int, Int) -> (Int -> next) -> RandomF next

makeFunctorInstance ''RandomF

type RandomL next = Free RandomF next

class Random m where
    getRandomInt :: (Int,Int) -> m Int

instance Random (Free RandomF) where
    getRandomInt range = liftF $ GetRandomInt range id
