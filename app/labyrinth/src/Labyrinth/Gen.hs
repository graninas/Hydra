module Labyrinth.Gen where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List

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

generateExits :: Bounds -> Int -> Labyrinth -> LangL Labyrinth
generateExits bounds@(xSize, ySize) cnt lab = do
  edgeTags <- replicateM (cnt * 5) (toEnum <$> getRandomInt (0, 3))
  exits'   <- mapM toExit edgeTags
  pure $ placeExits lab $ take cnt $ List.nub exits'
  where
    toExit :: Direction -> LangL (Direction, Int, Int)
    toExit DirUp    = (DirUp,,0)           <$> getRandomInt (0, xSize - 1)
    toExit DirDown  = (DirDown,,ySize-1)   <$> getRandomInt (0, xSize - 1)
    toExit DirLeft  = (DirLeft,0,)         <$> getRandomInt (0, ySize - 1)
    toExit DirRight = (DirRight, xSize-1,) <$> getRandomInt (0, ySize - 1)

    placeExits :: Labyrinth -> [(Direction, Int, Int)] -> Labyrinth
    placeExits lab []  = lab
    placeExits lab ((dir,x,y):ps) = case Map.lookup (x,y) lab of
      Nothing -> error $ "placeExits: Cell not found: " <> show (x,y)
      Just (c, cont) -> placeExits (Map.insert (x,y) (setExit c dir, cont) lab) ps

generateWormholes :: Bounds -> Int -> Labyrinth -> LangL Labyrinth
generateWormholes bounds@(xSize, ySize) cnt lab = do
  xs <- replicateM cnt $ getRandomInt (0, xSize - 1)
  ys <- replicateM cnt $ getRandomInt (0, ySize - 1)
  let ps = zip xs ys
  pure $ placeWormholes lab 0 ps
  where
    placeWormholes :: Labyrinth -> Int -> [Pos] -> Labyrinth
    placeWormholes lab _ [] = lab
    placeWormholes lab idx (p:ps) =
      placeWormholes (Map.update (placeWormhole idx) p lab) (idx + 1) ps
    placeWormhole :: Int -> (Cell, Content) -> Maybe (Cell, Content)
    placeWormhole idx (c, _) = Just (c, Wormhole idx)

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
  case wDirs of
    [] -> backtrack bounds lab pathVar
    ws -> do
      rndWIdx <- toEnum <$> getRandomInt (0, length ws - 1)
      -- probabilty of walls removing to the visited cell
      chance <- getRandomInt (0, 100)
      let rndWDir = ws !! rndWIdx
      let p'      = calcNextPos p rndWDir
      let pSet'   = Set.insert p' pSet
      case (Set.member p' pSet, chance < 10) of
        (False, _) -> do
          lab' <- removeWalls' lab p rndWDir
          evalIO $ writeIORef pathVar (p', p:ps, pSet')
          generatePaths' bounds lab' pathVar
        (True, True) -> do
          lab' <- removeWalls' lab p rndWDir
          evalIO $ writeIORef pathVar (p, ps, pSet)
          generatePaths' bounds lab' pathVar
        (True, False) -> do
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
  ySize <- getRandomInt (4, 10)
  exits <- getRandomInt (1, 4)
  wormholes <- getRandomInt (2, 5)
  let bounds = (xSize, ySize)
  generateGrid bounds
    >>= generatePaths bounds
    >>= generateExits bounds exits
    >>= generateWormholes bounds wormholes
    -- >>= generateTreasure
