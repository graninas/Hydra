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
  deriving (Show, Read, Eq, Bounded, Enum)

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
