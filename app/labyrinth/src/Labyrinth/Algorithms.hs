module Labyrinth.Algorithms where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Labyrinth.Prelude
import Labyrinth.Domain


data LabyrinthInfo = LabyrinthInfo
  { _bounds    :: Bounds
  , _wormholes :: Wormholes
  , _exits     :: Exits
  , _treasure  :: Maybe Pos
  }

emptyLabyrinthInfo :: LabyrinthInfo
emptyLabyrinthInfo = LabyrinthInfo (0, 0) Map.empty Set.empty Nothing

calcNextPos :: Pos -> Direction -> Pos
calcNextPos (x, y) DirUp    = (x, y - 1)
calcNextPos (x, y) DirDown  = (x, y + 1)
calcNextPos (x, y) DirLeft  = (x - 1, y)
calcNextPos (x, y) DirRight = (x + 1, y)

increaseBounds :: Bounds -> Pos -> Bounds
increaseBounds (x', y') (x, y) = (max x' (x + 1), max y' (y + 1))

increaseBounds' :: Pos -> LabyrinthInfo -> LabyrinthInfo
increaseBounds' (x, y) labInfo =
  let (x', y') = _bounds labInfo
  in labInfo { _bounds = (max x' (x + 1), max y' (y + 1)) }

analyzeLabyrinth :: Labyrinth -> LabyrinthInfo
analyzeLabyrinth lab = Map.foldrWithKey f emptyLabyrinthInfo lab
  where
    f :: Pos -> (Cell, Content) -> LabyrinthInfo -> LabyrinthInfo
    f p (cell, content) = increaseBounds' p . analyzeCell p cell . analyzeContent p content

analyzeContent :: Pos -> Content -> LabyrinthInfo -> LabyrinthInfo
analyzeContent p (Wormhole n) labInfo
  = labInfo { _wormholes = Map.insert n p $ _wormholes labInfo }
analyzeContent p Treasure labInfo = labInfo { _treasure = Just p }
analyzeContent _ _ labInfo = labInfo

analyzeCell :: Pos -> Cell -> LabyrinthInfo -> LabyrinthInfo
analyzeCell p (Cell l r u d)
  = analyzeExit p DirLeft l
  . analyzeExit p DirRight r
  . analyzeExit p DirUp u
  . analyzeExit p DirDown d

analyzeExit :: Pos -> Direction -> Wall -> LabyrinthInfo -> LabyrinthInfo
analyzeExit p dir (Monolith True) labInfo = labInfo { _exits = Set.insert (p, dir) $ _exits labInfo }
analyzeExit _ _ _ labInfo = labInfo


oppositeDir :: Direction -> Direction
oppositeDir DirUp = DirDown
oppositeDir DirDown = DirUp
oppositeDir DirLeft = DirRight
oppositeDir DirRight = DirLeft

isWall :: Wall -> Bool
isWall Wall = True
isWall _ = False

isMonolith :: Wall -> Bool
isMonolith (Monolith _) = True
isMonolith _ = False

isWallOnDirection :: Cell -> Direction -> Bool
isWallOnDirection (Cell _ _ u _) DirUp    = isWall u || isMonolith u
isWallOnDirection (Cell _ _ _ d) DirDown  = isWall d || isMonolith d
isWallOnDirection (Cell l _ _ _) DirLeft  = isWall l || isMonolith l
isWallOnDirection (Cell _ r _ _) DirRight = isWall r || isMonolith r


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
onBounds (_, _) (_, y) DirUp    = y - 1 <= 0
onBounds (_, ySize) (_, y) DirDown  = y + 1 >= ySize
onBounds (_, _) (x, _) DirLeft  = x - 1 <= 0
onBounds (xSize, _) (x, _) DirRight = x + 1 >= xSize

inBounds :: Bounds -> Pos -> Bool
inBounds (xSize, ySize) (x, y) = x >= 0 && x < xSize && y >= 0 && y < ySize
