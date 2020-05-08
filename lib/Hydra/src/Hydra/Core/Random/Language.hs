{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module Hydra.Core.Random.Language where

import           Hydra.Prelude

import           Hydra.Core.Random.Class


-- | Language for Random.
data RandomF next where
  -- | Get Int from range
  GetRandomInt :: (Int, Int) -> (Int -> next) -> RandomF next

instance Functor RandomF where
  fmap f (GetRandomInt range next) = GetRandomInt range (f . next)


type RandomL = Free RandomF

instance Random RandomL where
  getRandomInt range = liftF $ GetRandomInt range id
