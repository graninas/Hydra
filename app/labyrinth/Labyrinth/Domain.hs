module Labyrinth.Domain where

import Hyrda.Prelude

import qualified Data.Text as T
import qualified Data.Map as Map

data Wall
  = NoWall
  | Wall
  | Monolith
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

type Labyrinth = Map.Map (Int, Int) (Cell, Content)
