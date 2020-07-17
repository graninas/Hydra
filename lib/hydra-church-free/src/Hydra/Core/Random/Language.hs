module Hydra.Core.Random.Language where

import           Hydra.Prelude

import qualified Hydra.Core.Random.Class as C


-- | Language for Random.
data RandomF next where
  -- | Get Int from range
  GetRandomInt :: (Int, Int) -> (Int -> next) -> RandomF next

instance Functor RandomF where
  fmap f (GetRandomInt range next) = GetRandomInt range (f . next)


type RandomL = F RandomF

instance C.Random RandomL where
  getRandomInt range = liftFC $ GetRandomInt range id
