module Labyrinth.Algorithms where

import qualified Data.Map      as Map

import Labyrinth.Prelude
import Labyrinth.Domain


calcNextPos :: Pos -> Direction -> Pos
calcNextPos (x, y) DirUp    = (x, y - 1)
calcNextPos (x, y) DirDown  = (x, y + 1)
calcNextPos (x, y) DirLeft  = (x - 1, y)
calcNextPos (x, y) DirRight = (x + 1, y)

increaseBounds :: Bounds -> Pos -> Bounds
increaseBounds (x', y') (x, y) = (max x' (x + 1), max y' (y + 1))

analyzeLabyrinth :: Labyrinth -> (Bounds, Wormholes)
analyzeLabyrinth lab = Map.foldrWithKey f ((0, 0), Map.empty) lab
  where
    f :: Pos -> (Cell, Content) -> (Bounds, Wormholes) -> (Bounds, Wormholes)
    f pos (_, Wormhole n) (bounds, wormholes) = (increaseBounds bounds pos, Map.insert n pos wormholes)
    f pos _ (bounds, wormholes) = (increaseBounds bounds pos, wormholes)

oppositeDir :: Direction -> Direction
oppositeDir DirUp = DirDown
oppositeDir DirDown = DirUp
oppositeDir DirLeft = DirRight
oppositeDir DirRight = DirLeft

isWall :: Wall -> Bool
isWall Wall = True
isWall _ = False

removeWall' :: Cell -> Direction -> Cell
removeWall' c dir = snd $ removeWall c dir

removeWall :: Cell -> Direction -> (Bool, Cell)
removeWall (Cell l r u d) DirUp    = (isWall u, Cell l r NoWall d)
removeWall (Cell l r u d) DirDown  = (isWall d, Cell l r u NoWall)
removeWall (Cell l r u d) DirLeft  = (isWall l, Cell NoWall r u d)
removeWall (Cell l r u d) DirRight = (isWall r, Cell l NoWall u d)

setExit :: Cell -> Direction -> Cell
setExit (Cell l r _ d) DirUp    = Cell l r (Monolith True) d
setExit (Cell l r u _) DirDown  = Cell l r u (Monolith True)
setExit (Cell _ r u d) DirLeft  = Cell (Monolith True) r u d
setExit (Cell l _ u d) DirRight = Cell l (Monolith True) u d

onBounds :: Bounds -> Pos -> Direction -> Bool
onBounds (_, ySize) (x, y) DirUp    = y - 1 <= 0
onBounds (_, ySize) (x, y) DirDown  = y + 1 >= ySize
onBounds (xSize, _) (x, y) DirLeft  = x - 1 <= 0
onBounds (xSize, _) (x, y) DirRight = x + 1 >= xSize
