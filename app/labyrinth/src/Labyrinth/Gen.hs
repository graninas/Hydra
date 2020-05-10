module Labyrinth.Gen where

import qualified Data.Map      as Map

import Labyrinth.Prelude
import Labyrinth.Domain
import Labyrinth.Types

generateLabyrinth :: LangL Labyrinth
generateLabyrinth = throwException $ NotImplemented "Generation"


analyzeLabyrinth :: Labyrinth -> (Bounds, Wormholes)
analyzeLabyrinth lab = Map.foldrWithKey f ((0, 0), Map.empty) lab
  where
    increaseBounds :: Bounds -> Pos -> Bounds
    increaseBounds (x', y') (x, y) = (max x' (x + 1), max y' (y + 1))
    f :: Pos -> (Cell, Content) -> (Bounds, Wormholes) -> (Bounds, Wormholes)
    f pos (_, Wormhole n) (bounds, wormholes) = (increaseBounds bounds pos, Map.insert n pos wormholes)
    f pos _ (bounds, wormholes) = (increaseBounds bounds pos, wormholes)
