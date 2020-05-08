module Hydra.Core.Random.Interpreter where

import           Hydra.Prelude

import           System.Random       hiding (next)

import qualified Hydra.Core.Language as L

-- | Interpret RandomF language.
interpretRandomF :: L.RandomF a -> IO a
interpretRandomF (L.GetRandomInt range next) = do
    r <- randomRIO range
    pure $ next r

-- | Interpret RandomL language.
runRandomL :: L.RandomL a -> IO a
runRandomL = foldFree interpretRandomF
