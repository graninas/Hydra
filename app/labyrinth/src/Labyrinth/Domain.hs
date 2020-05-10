module Labyrinth.Domain where

import qualified Data.Map as Map

import Labyrinth.Prelude as L

type Pos = (Int, Int)
type Bounds = (Int, Int)
type Wormholes = Map Int Pos

type LabRender = (Bounds, Map Pos String)
type Skeleton = LabRender

data Direction
  = DirUp
  | DirDown
  | DirLeft
  | DirRight
  deriving (Show, Read, Eq)

data Wall
  = NoWall
  | Wall
  | Monolith Bool     -- ^ On True, then it's an exit
  deriving (Show, Read, Eq)

data Cell = Cell
  { leftWall  :: Wall
  , rightWall :: Wall
  , upWall    :: Wall
  , downWall  :: Wall
  }
  deriving (Show, Read, Eq)

data Content
  = NoContent
  | Treasure
  | Wormhole Int
  deriving (Show, Read, Eq)

type Labyrinth = Map Pos (Cell, Content)

increaseBounds :: Bounds -> Pos -> Bounds
increaseBounds (x', y') (x, y) = (max x' (x + 1), max y' (y + 1))

analyzeLabyrinth :: Labyrinth -> (Bounds, Wormholes)
analyzeLabyrinth lab = Map.foldrWithKey f ((0, 0), Map.empty) lab
  where
    f :: Pos -> (Cell, Content) -> (Bounds, Wormholes) -> (Bounds, Wormholes)
    f pos (_, Wormhole n) (bounds, wormholes) = (increaseBounds bounds pos, Map.insert n pos wormholes)
    f pos _ (bounds, wormholes) = (increaseBounds bounds pos, wormholes)
