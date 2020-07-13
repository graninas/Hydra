{-|
Attributes regarding the the shape of labyrinth,
such as walls and exits, and objects placed within
the labyrinth, such as Bears, Treasures, and The Map.
-}


module Labyrinth.Gen where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List

import Labyrinth.Prelude
import Labyrinth.Domain
import Labyrinth.Types
import Labyrinth.Algorithms
import Labyrinth.Render
import Labyrinth.App

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

-- | Generates a treasure.
generateTreasure :: Labyrinth -> LangL Labyrinth
generateTreasure lab = do
  let emptyCells = getEmptyCells lab
  when (Set.null emptyCells) $ throwException $ GenerationError "No empty cells found to place the treasure."
  rndPosIdx <- getRandomInt (0, Set.size emptyCells - 1)
  let pos = Set.elemAt rndPosIdx emptyCells
  case Map.lookup pos lab of
    Just (c, NoContent) -> pure $ Map.insert pos (c, Treasure) lab
    Just _              -> throwException $ GenerationError $ "Unexpected non empty cell found: " <> show pos
    Nothing             -> throwException $ GenerationError "Unable to obtain a cell for the treasure."




generateTheMap :: AppState -> (Int, Int) -> [(Int, Int)] ->
generateTheMap appState pos trailList = do
  let trailPointsVar = _labTrailpoints appState
  let labyrinthVar   = _labyrinth appState

  trailPoints :: Map Pos (Cell, Content) <- readVarIO trailPointsVar
  lab         :: Map Pos (Cell, Content) <- readVarIO labyrinthVar

  cellRender :: Pos -> (Cell, Content) -> LabRender -> LabRender
  cellRender (x0, y0) (cell, content) (bounds, labRender)  where
    resUp    = if (App.testMove pos DirUp lab = (SuccessfullMove OR LeavingLabyrinthMove))
               then PassageOption else NoPassageOption
    resDown  = if App.testMove pos DirDown lab = (SuccessfullMove OR LeavingLabyrinthMove))
               then PassageOption else NoPassageOption
    resLeft  = if App.testMove pos DirLeft lab = (SuccessfullMove OR LeavingLabyrinthMove))
               then PassageOption else NoPassageOption
    resRight = if App.testMove pos DirRight lab = (SuccessfullMove OR LeavingLabyrinthMove))
               then PassageOption else NoPassageOption

    visual   = (resUp, resDown, resLeft, resRight)


-- | Generates the map.



  case maybeLabCell of
    Nothing -> error $ "The cell is not found on pos" ++ show pos
    Just (cell, _) -> do
      let n = nextTrailpoint trailPoints
      let newTrailpoints = Map.insert pos (cell, Trailpoint n) trailPoints
      writeVarIO trailPointsVar newTrailpoints


  let

  emptyCells = getEmptyCells lab
  when (Set.null emptyCells) $ throwException $ GenerationError "No empty cells found to place the map."
  rndPosIdx <- getRandomInt (0, Set.size emptyCells - 1)
  let pos = Set.elemAt rndPosIdx emptyCells
  case Map.lookup pos lab of
    Just (c, NoContent) -> pure $ Map.insert pos (c, TheMap) lab
    Just _              -> throwException $ GenerationError $ "Unexpected non empty cell found: " <> show pos
    Nothing             -> throwException $ GenerationError "Unable to obtain a cell for the map."




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

generateWormholes :: Int -> Labyrinth -> LangL Labyrinth
generateWormholes cnt lab = do
  let emptyCells = getEmptyCells lab
  placeWormholes cnt emptyCells lab

placeWormholes :: Int -> Set.Set Pos -> Labyrinth -> LangL Labyrinth
placeWormholes cnt emptyCells lab
    | Set.null emptyCells || cnt == 0 = pure lab
    | otherwise = do
  rndPosIdx <- getRandomInt (0, Set.size emptyCells - 1)
  let pos = Set.elemAt rndPosIdx emptyCells
  case Map.lookup pos lab of
    Just (c, NoContent) -> do
      let lab'        = Map.insert pos (c, Wormhole cnt) lab
      let emptyCells' = Set.deleteAt rndPosIdx emptyCells
      placeWormholes  (cnt - 1) emptyCells' lab'
    _ -> pure lab

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
  lab4 <- generateTreasure lab3
  lab5 <- generateTheMap lab4
  lab6 <- generateWormholes wormholes lab5
  pure lab6
