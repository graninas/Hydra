module Labyrinth.Domain where

import qualified Data.Text as T
import qualified Data.Map as Map

import Labyrinth.Prelude as L

type Pos = (Int, Int)
type Bounds = (Int, Int)


data Direction = DirUp | DirDown | DirLeft | DirRight
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

type Labyrinth = Map.Map Pos (Cell, Content)
