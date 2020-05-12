module Labyrinth.Gen where

import qualified Data.Map as Map

import Labyrinth.Prelude
import Labyrinth.Domain
import Labyrinth.Types
import Labyrinth.Algorithms
import Labyrinth.Render

generateGrid :: Bounds -> LangL Labyrinth
generateGrid (xSize, ySize) = pure $ Map.fromList $ do
  x' <- [0..xSize-1]
  y' <- [0..ySize-1]
  let leftW  = if x' == 0       then (Monolith False) else Wall
  let rightW = if x' == xSize-1 then (Monolith False) else Wall
  let upW    = if y' == 0       then (Monolith False) else Wall
  let downW  = if y' == ySize-1 then (Monolith False) else Wall
  pure ((x', y'), (Cell leftW rightW upW downW, NoContent))

generatePaths :: Bounds -> Labyrinth -> LangL Labyrinth
generatePaths bounds@(xSize, ySize) grid = do
  cellsLeftVar <- evalIO $ newIORef [(x, y) | x <- [0..xSize-1], y <- [0..ySize-1]]
  generatePaths' bounds cellsLeftVar grid

getWallDirs :: Cell -> [Direction]
getWallDirs (Cell l r u d)
  =  [ DirLeft  | isWall l ]
  <> [ DirRight | isWall r ]
  <> [ DirUp    | isWall u ]
  <> [ DirDown  | isWall d ]

generatePaths'
  :: Bounds
  -> IORef [Pos]
  -> Labyrinth
  -> LangL Labyrinth
generatePaths' bounds cellsLeftVar lab = do
  mbNextCellPos <- getNextCellPos cellsLeftVar
  let mbNextCell = mbNextCellPos >>= (\pos -> (pos, ) <$> Map.lookup pos lab)
  case mbNextCell of
    Nothing -> pure lab
    Just (pos, (c, cnt)) -> case getWallDirs c of
      [] -> do
        popCell cellsLeftVar
        generatePaths' bounds cellsLeftVar lab
      dirs -> do
        nextDir <- (dirs !!) <$> getRandomInt (0, length dirs - 1)
        lab' <- removeWalls' bounds lab pos nextDir
        popCell cellsLeftVar
        generatePaths' bounds cellsLeftVar lab'

-- generatePaths'
--   :: Bounds
--   -> IORef [Pos]
--   -> Labyrinth
--   -> LangL Labyrinth
-- generatePaths' bounds cellsLeftVar lab = do
--   mbNextCell <- getNextCellPos cellsLeftVar
--   nextDir    <- getRndDirection
--   let onBounds' pos = onBounds bounds pos nextDir
--   case mbNextCell of
--     Nothing -> pure lab
--     Just pos
--       | onBounds' pos -> generatePaths' bounds cellsLeftVar lab
--       | otherwise     -> do
--           mbLab' <- removeWalls bounds lab pos nextDir
--           case mbLab' of
--             Nothing -> generatePaths' bounds cellsLeftVar lab
--             Just lab' -> do
--                 popCell cellsLeftVar
--                 generatePaths' bounds cellsLeftVar lab'

getNextCellPos :: IORef [Pos] -> LangL (Maybe Pos)
getNextCellPos cellsLeftVar = do
  cellsLeft <- evalIO $ readIORef cellsLeftVar
  case cellsLeft of
    [] -> pure Nothing
    (c:_) -> pure $ Just c

popCell :: IORef [Pos] -> LangL ()
popCell cellsLeftVar = do
  cellsLeft <- evalIO $ readIORef cellsLeftVar
  case cellsLeft of
    [] -> pure ()
    (_:cs) -> evalIO $ writeIORef cellsLeftVar cs


removeWalls' :: Bounds -> Labyrinth -> Pos -> Direction -> LangL Labyrinth
removeWalls' bounds lab pos dir = do
  let coPos = calcNextPos pos dir
  let coDir = oppositeDir dir
  let mbC1 = Map.lookup pos lab
  let mbC2 = Map.lookup coPos lab
  case (mbC1, mbC2) of
    (Just (c1, cnt1), Just (c2, cnt2)) -> do
      let noWallC1 = removeWall' c1 dir
      let noWallC2 = removeWall' c2 coDir
      pure
        $ Map.insert pos   (noWallC1, cnt1)
        $ Map.insert coPos (noWallC2, cnt2) lab
    _ -> do
      printLabyrinth lab
      throwException
        $ InvalidOperation
        $ "removeWalls: Cells not found. pos="
          <> show pos
          <> ", coPos="
          <> show coPos
          <> ", dir="
          <> show dir
          <> ", coDir="
          <> show coDir
          <> ", cells:"
          <> show (mbC1, mbC2)
          <> ", bounds="
          <> show bounds

removeWalls :: Bounds -> Labyrinth -> Pos -> Direction -> LangL (Maybe Labyrinth)
removeWalls bounds lab pos dir = do
  let coPos = calcNextPos pos dir
  let coDir = oppositeDir dir
  let mbC1 = Map.lookup pos lab
  let mbC2 = Map.lookup coPos lab
  case (mbC1, mbC2) of
    (Just (c1, cnt1), Just (c2, cnt2)) -> do
      let (removed, noWallC1) = removeWall c1 dir
      let (_, noWallC2)       = removeWall c2 coDir
      if removed
          then pure $ Just
            $ Map.insert pos   (noWallC1, cnt1)
            $ Map.insert coPos (noWallC2, cnt2) lab
      else pure Nothing
    _ -> do
      printLabyrinth lab
      throwException
        $ InvalidOperation
        $ "removeWalls: Cells not found. pos="
          <> show pos
          <> ", coPos="
          <> show coPos
          <> ", dir="
          <> show dir
          <> ", coDir="
          <> show coDir
          <> ", cells:"
          <> show (mbC1, mbC2)
          <> ", bounds="
          <> show bounds


getRndDirection :: LangL Direction
getRndDirection = toEnum <$> getRandomInt (0, 3)

generateLabyrinth :: LangL Labyrinth
generateLabyrinth = do
  xSize <- getRandomInt (4, 10)
  -- ySize <- getRandomInt (4, 10)
  exits <- getRandomInt (1, 4)
  wormholes <- getRandomInt (2, 5)
  generateGrid (xSize, xSize)
    >>= generatePaths (xSize, xSize)
    -- >>= generateExits exits
    -- >>= generateWormholes wormholes
    -- >>= generateTreasure
