module Labyrinth.Gen where

import qualified Data.Map as Map
import qualified Data.Set as Set

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
  let startCell = (0, 0)
  pathVar    <- evalIO $ newIORef (startCell, [], Set.singleton startCell)
  generatePaths' bounds grid pathVar

getWallDirs :: Labyrinth -> Pos -> [Direction]
getWallDirs lab pos = case Map.lookup pos lab of
  Nothing -> []
  Just (Cell l r u d, _) ->
    [ DirLeft  | isWall l ]
    <> [ DirRight | isWall r ]
    <> [ DirUp    | isWall u ]
    <> [ DirDown  | isWall d ]

backtrack
  :: Bounds
  -> Labyrinth
  -> IORef (Pos, [Pos], Set.Set Pos)
  -> LangL Labyrinth
backtrack bounds lab pathVar = do
  (p, ps, pSet) <- evalIO $ readIORef pathVar
  case ps of
    [] -> pure lab
    (p' : ps') -> do
      evalIO $ writeIORef pathVar (p', ps', pSet)
      generatePaths' bounds lab pathVar

generatePaths'
  :: Bounds
  -> Labyrinth
  -> IORef (Pos, [Pos], Set.Set Pos)
  -> LangL Labyrinth
generatePaths' bounds lab pathVar = do
  (p, ps, pSet) <- evalIO $ readIORef pathVar
  let wDirs = getWallDirs lab p
  putStrLn $ "G:> " <> show p <> ", " <> show (length ps) <> ", "
    <> show wDirs <> ", " <> show (Set.size pSet)

  case wDirs of
    []     -> backtrack bounds lab pathVar
    ws -> do
      rndWIdx <- toEnum <$> getRandomInt (0, length ws - 1)
      let rndWDir = ws !! rndWIdx
      let p'      = calcNextPos p rndWDir
      let pSet'   = Set.insert p' pSet
      case Set.member p' pSet of
        False -> do
          lab' <- removeWalls' lab p rndWDir
          evalIO $ writeIORef pathVar (p', p:ps, pSet')
          generatePaths' bounds lab' pathVar
        True -> do
          evalIO $ writeIORef pathVar (p, ps, pSet)
          backtrack bounds lab pathVar



removeWalls' :: Labyrinth -> Pos -> Direction -> LangL Labyrinth
removeWalls' lab pos dir = do
  let coPos = calcNextPos pos dir
  let coDir = oppositeDir dir
  let mbC1 = Map.lookup pos lab
  let mbC2 = Map.lookup coPos lab
  case (mbC1, mbC2) of
    (Just (c1, cnt1), Just (c2, cnt2)) -> do
      let noWallC1 = removeWall' c1 dir
      let noWallC2 = removeWall' c2 coDir
      let lab' = Map.insert pos   (noWallC1, cnt1)
               $ Map.insert coPos (noWallC2, cnt2) lab
      pure lab'
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
