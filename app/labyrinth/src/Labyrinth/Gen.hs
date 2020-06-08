module Labyrinth.Gen where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List

import Labyrinth.Prelude
import Labyrinth.Domain
import Labyrinth.Types
import Labyrinth.Algorithms
import Labyrinth.Render

type Chance = Int

getRandomDirection :: LangL Direction
getRandomDirection = toEnum <$> getRandomInt (0, 3)

-- | Generates a full grid of cells with walls.
generateGrid :: Bounds -> LangL Labyrinth
generateGrid (xSize, ySize) = pure $ Map.fromList $ do
  x' <- [0..xSize-1]
  y' <- [0..ySize-1]
  let leftW  = if x' == 0       then (Monolith False) else Wall
  let rightW = if x' == xSize-1 then (Monolith False) else Wall
  let upW    = if y' == 0       then (Monolith False) else Wall
  let downW  = if y' == ySize-1 then (Monolith False) else Wall
  pure ((x', y'), (Cell leftW rightW upW downW, NoContent))

-- | Starts generating the labyrinth itself by removing random walls.
-- Uses a depth-first algorithm.
generatePaths :: Bounds -> Labyrinth -> LangL Labyrinth
generatePaths bounds@(xSize, ySize) grid = do
  let lst = [(x,y) | x <- [0..xSize - 1], y <- [0..ySize - 1]]
  -- Max probability of a wall leading to the nearest visited path.
  -- When 100, removes a lot of walls and makes a lot of loops.
  -- When 10, removes a small amount of walls => creates a few loops.
  let wallMaxChance = 10
  visitedVar    <- evalIO $ newIORef Set.empty
  nonVisitedVar <- evalIO $ newIORef $ Set.fromList lst
  startPathGeneration grid bounds wallMaxChance visitedVar nonVisitedVar

-- | Generates a new path.
startPathGeneration
  :: Labyrinth
  -> Bounds
  -> Chance
  -> IORef (Set.Set Pos)
  -> IORef (Set.Set Pos)
  -> LangL Labyrinth
startPathGeneration lab bounds chance visitedVar nonVisitedVar = do
  nonVisited <- evalIO $ readIORef nonVisitedVar
  case Set.lookupMin nonVisited of
    Nothing -> pure lab
    Just p  -> do
      pathVar <- evalIO $ newIORef (p, [])
      lab' <- generatePathway bounds lab chance pathVar visitedVar nonVisitedVar
      -- Generate the next path starting from a non-visited cell.
      -- 100% probabilty to make a wall to the already visited path.
      startPathGeneration lab' bounds 100 visitedVar nonVisitedVar

-- | Falls back to the previous cell of the path.
backtrack
  :: Bounds
  -> Labyrinth
  -> Chance
  -> IORef (Pos, [Pos])
  -> IORef (Set.Set Pos)
  -> IORef (Set.Set Pos)
  -> LangL Labyrinth
backtrack bounds lab maxChance pathVar visitedVar nonVisitedVar = do
  (_, ps) <- evalIO $ readIORef pathVar
  case ps of
    []         -> pure lab
    (p' : ps') -> do
      evalIO $ writeIORef pathVar (p', ps')
      generatePathway bounds lab maxChance pathVar visitedVar nonVisitedVar

-- | Generates a pathway (the algorithm)
generatePathway
  :: Bounds
  -> Labyrinth
  -> Chance
  -> IORef (Pos, [Pos])
  -> IORef (Set.Set Pos)
  -> IORef (Set.Set Pos)
  -> LangL Labyrinth
generatePathway bounds lab maxChance pathVar visitedVar nonVisitedVar = do
  (p, ps) <- evalIO $ readIORef pathVar
  evalIO $ modifyIORef' visitedVar    (Set.insert p)
  evalIO $ modifyIORef' nonVisitedVar (Set.delete p)
  visited <- evalIO $ readIORef visitedVar
  let wDirs = getWallDirs lab p
  case wDirs of
    [] -> backtrack bounds lab maxChance pathVar visitedVar nonVisitedVar
    _ -> do
      rndWIdx <- getRandomInt (0, length wDirs - 1)
      -- probabilty of walls removing to the visited cell
      chance <- getRandomInt (0, 100)
      let rndWDir = wDirs !! rndWIdx
      let p'      = calcNextPos p rndWDir
      case (Set.member p' visited, chance < maxChance) of
        -- Next cell not visited, removing wall
        (False, _) -> do
          lab' <- removeWalls' lab p rndWDir
          evalIO $ writeIORef pathVar (p', p:ps)
          generatePathway bounds lab' maxChance pathVar visitedVar nonVisitedVar
        -- Next cell is visited already, but still removing wall
        (True, True) -> do
          lab' <- removeWalls' lab p rndWDir
          evalIO $ writeIORef pathVar (p, ps)
          generatePathway bounds lab' maxChance pathVar visitedVar nonVisitedVar
        -- Next cell is visited, do not remove wall
        (True, False) -> do
          evalIO $ writeIORef pathVar (p, ps)
          backtrack bounds lab maxChance pathVar visitedVar nonVisitedVar


generateTreasure :: Bounds -> Labyrinth -> LangL Labyrinth
generateTreasure (xSize, ySize) lab = generateTreasure' 10
  where
    generateTreasure' :: Int -> LangL Labyrinth
    generateTreasure' 0 = throwException $ GenerationError "Unable to place treasure after 10 tries"
    generateTreasure' n = do
      x <- getRandomInt (0, xSize - 1)
      y <- getRandomInt (0, ySize - 1)
      case Map.lookup (x,y) lab of
        Nothing -> throwException $ GenerationError $ "Cell not found: " <> show (x, y)
        Just (c, NoContent) -> pure $ Map.insert (x, y) (c, Treasure) lab
        _ -> generateTreasure' (n - 1)

generateExits :: Bounds -> Int -> Labyrinth -> LangL Labyrinth
generateExits (xSize, ySize) cnt lab = do
  edgeTags <- replicateM (cnt * 5) (toEnum <$> getRandomInt (0, 3))
  exits'   <- mapM toExit edgeTags
  placeExits lab $ take cnt $ List.nub exits'
  where
    toExit :: Direction -> LangL (Direction, Int, Int)
    toExit DirUp    = (DirUp,,0)           <$> getRandomInt (0, xSize - 1)
    toExit DirDown  = (DirDown,,ySize-1)   <$> getRandomInt (0, xSize - 1)
    toExit DirLeft  = (DirLeft,0,)         <$> getRandomInt (0, ySize - 1)
    toExit DirRight = (DirRight, xSize-1,) <$> getRandomInt (0, ySize - 1)

    placeExits :: Labyrinth -> [(Direction, Int, Int)] -> LangL Labyrinth
    placeExits lab' [] = pure lab'
    placeExits lab' ((dir,x,y):ps) = case Map.lookup (x,y) lab' of
      Nothing        -> throwException $ GenerationError $ "placeExits: Cell not found: " <> show (x,y)
      Just (c, cont) -> placeExits (Map.insert (x,y) (setExit c dir, cont) lab') ps

generateWormholes :: Bounds -> Int -> Labyrinth -> LangL Labyrinth
generateWormholes (xSize, ySize) cnt lab = do
  xs <- replicateM (cnt * 5) $ getRandomInt (0, xSize - 1)
  ys <- replicateM (cnt * 5) $ getRandomInt (0, ySize - 1)
  pure $ placeWormholes lab 0 $ take cnt $ List.nub $ zip xs ys
  where
    placeWormholes :: Labyrinth -> Int -> [Pos] -> Labyrinth
    placeWormholes lab' _ [] = lab'
    placeWormholes lab' idx (p:ps) =
      placeWormholes (Map.update (placeWormhole idx) p lab') (idx + 1) ps
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
        $ GenerationError
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

validateBoundsOrFail :: Bounds -> Int -> LangL ()
validateBoundsOrFail (x, y) maxSize = do
  let failCond = x <= 0 || y <= 0 || x > maxSize || y > maxSize
  when failCond $ throwException $ GenerationError $ "Bounds not supported: " <> show (x, y)

generateRndLabyrinth :: LangL Labyrinth
generateRndLabyrinth = do
  xSize     <- getRandomInt (4, 10)
  ySize     <- getRandomInt (4, 10)
  exits     <- getRandomInt (1, 4)
  wormholes <- getRandomInt (2, 5)
  generateLabyrinth (xSize, ySize) exits wormholes

type Tries = Int

generateLabyrinth :: Bounds -> Int -> Int -> LangL Labyrinth
generateLabyrinth bounds exits wormholes = do
  validateBoundsOrFail bounds 10
  lab1 <- generateGrid bounds
  lab2 <- generatePaths bounds lab1
  lab3 <- generateExits bounds exits lab2
  lab4 <- generateTreasure bounds lab3
  lab5 <- generateWormholes bounds wormholes lab4
  pure lab5
