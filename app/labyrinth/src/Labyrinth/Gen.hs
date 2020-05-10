module Labyrinth.Gen where

import qualified Data.Map      as Map

import Labyrinth.Prelude
import Labyrinth.Domain
import Labyrinth.Types

generateGrid :: Int -> Int -> LangL Labyrinth
generateGrid xSize ySize = pure $ Map.fromList $ do
  x' <- [0..xSize-1]
  y' <- [0..ySize-1]
  let leftW  = if x' == 0       then (Monolith False) else Wall
  let rightW = if x' == xSize-1 then (Monolith False) else Wall
  let upW    = if y' == 0       then (Monolith False) else Wall
  let downW  = if y' == ySize-1 then (Monolith False) else Wall
  pure ((x', y'), (Cell leftW rightW upW downW, NoContent))

generatePaths :: Int -> Int -> Labyrinth -> LangL Labyrinth
generatePaths xSize ySize grid = do
  cellsLeftVar <- evalIO $ newIORef [(x, y) | x <- [0..xSize], y <- [0..ySize]]
  -- rndDirs      <- replicateM (xSize * ySize) (getRandomInt (0, 3))
  -- rndDirsVar   <- evalIO $ newIORef

  x <- getRandomInt (0, xSize - 1)
  y <- getRandomInt (0, ySize - 1)

  pure grid


generateLabyrinth :: LangL Labyrinth
generateLabyrinth = do
  xSize <- getRandomInt (4, 10)
  ySize <- getRandomInt (4, 10)
  exits <- getRandomInt (1, 4)
  wormholes <- getRandomInt (2, 5)
  generateGrid xSize ySize
    >>= generatePaths xSize ySize
    -- >>= generateExits exits
    -- >>= generateWormholes wormholes
    -- >>= generateTreasure
