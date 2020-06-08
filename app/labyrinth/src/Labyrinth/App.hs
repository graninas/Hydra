module Labyrinth.App where

import qualified Data.Map      as Map
import qualified Data.String   as Str

import qualified Hydra.Domain  as D
import Labyrinth.Prelude       as L
import Labyrinth.Domain
import Labyrinth.Types
import Labyrinth.Render
import Labyrinth.Algorithms
import Labyrinth.Gen
import Labyrinth.Lens
import qualified Labyrinth.KVDB.Model as KVDB
import qualified Labyrinth.KVDB.Repository as KVDB

data Passage
  = Passage
  | Exit
  | MonolithWall
  | RegularWall
  deriving (Show, Read, Eq)

data MovingResult
  = SuccessfullMove Pos
  | ImpossibleMove String
  | InvalidMove String
  | LeavingLabyrinthMove
  deriving (Show, Read, Eq)

getPassage :: Cell -> Direction -> Passage
getPassage (Cell _ _ NoWall _) DirUp    = Passage
getPassage (Cell _ _ _ NoWall) DirDown  = Passage
getPassage (Cell NoWall _ _ _) DirLeft  = Passage
getPassage (Cell _ NoWall _ _) DirRight = Passage

getPassage (Cell _ _ (Monolith True) _) DirUp    = Exit
getPassage (Cell _ _ _ (Monolith True)) DirDown  = Exit
getPassage (Cell (Monolith True) _ _ _) DirLeft  = Exit
getPassage (Cell _ (Monolith True) _ _) DirRight = Exit

getPassage (Cell _ _ (Monolith False) _) DirUp    = MonolithWall
getPassage (Cell _ _ _ (Monolith False)) DirDown  = MonolithWall
getPassage (Cell (Monolith False) _ _ _) DirLeft  = MonolithWall
getPassage (Cell _ (Monolith False) _ _) DirRight = MonolithWall

getPassage _ _ = RegularWall

leaveWithoutTreasure :: String
leaveWithoutTreasure = "You didn't find a treasure. Leaving means failure. Do you want to leave the labyrinth? (yes / no)"

winning :: String
winning = "Congratulations! You win!"

loosing :: String
loosing = "You lose..."

unknownCommand :: String -> String
unknownCommand cmdStr = "Unknown command: " <> cmdStr


-- Testing the move for a position. This function doesn't change the game state.
testMove :: Pos -> Direction -> Labyrinth -> MovingResult
testMove pos dir lab = res
  where
    nextPos    = calcNextPos pos dir
    mbCurCell  = Map.lookup pos lab
    mbNextCell = Map.lookup nextPos lab

    res = case mbCurCell of
      Nothing        -> InvalidMove $ "Cell not found: " +|| pos ||+ ""
      Just (cell, _) -> case (getPassage cell dir, mbNextCell) of
        (MonolithWall, _)    -> ImpossibleMove "Step impossible: monolith wall"
        (RegularWall, _)     -> ImpossibleMove "Step impossible: wall"
        (Passage, Nothing)   -> InvalidMove $ "Cell not found: " +|| nextPos ||+ ""
        (Passage, Just _)    -> SuccessfullMove nextPos
        (Exit, _)            -> LeavingLabyrinthMove

getPlayerPos :: AppState -> LangL Pos
getPlayerPos st = readVarIO $ st ^. playerPos

setPlayerPos :: AppState -> Pos -> LangL ()
setPlayerPos st newPos = writeVarIO (st ^. playerPos) newPos

getPlayerThreasureState :: AppState -> LangL Bool
getPlayerThreasureState st = readVarIO (st ^. playerInventory . treasureState)

setCellContent :: AppState -> Pos -> Content -> LangL ()
setCellContent st pos content = do
  lab <- readVarIO $ _labyrinth st
  case Map.lookup pos lab of
    Nothing        -> throwException $ InvalidOperation $ "Cell not found: " +|| pos ||+ ""
    Just (cell, _) -> writeVarIO (st ^. labyrinth) $ Map.insert pos (cell, content) lab

getCell :: AppState -> Pos -> LangL (Cell, Content)
getCell st pos = do
  lab <- readVarIO $ st ^. labyrinth
  case Map.lookup pos lab of
    Nothing -> throwException $ InvalidOperation $ "Cell not found: " +|| pos ||+ ""
    Just c -> pure c

setGameState :: AppState -> GameState -> LangL ()
setGameState st = writeVarIO (st ^. gameState)

getGameState :: AppState -> LangL GameState
getGameState st = readVarIO $ st ^. gameState

nextWormhole :: Wormholes -> Int -> Int
nextWormhole wms n | (n + 1 < Map.size wms) = n + 1
                   | otherwise = 0

executeWormhole :: AppState -> Int -> LangL ()
executeWormhole st prevWormhole = do
  wormholes <- readVarIO $ st ^. labWormholes
  let n = nextWormhole wormholes prevWormhole
  case Map.lookup n wormholes  of
    Nothing  -> throwException $ InvalidOperation $ "Wormhole not found: " +|| n ||+ ""
    Just pos -> setPlayerPos st pos

cancelPlayerLeaving :: AppState -> LangL ()
cancelPlayerLeaving st = do
  gameSt <- getGameState st
  case gameSt of
    PlayerIsAboutLeaving                 -> addGameMessage st "Okay, continue."
    PlayerIsAboutLossLeavingConfirmation -> addGameMessage st "Okay, continue."
    _ -> pure ()
  setGameState st PlayerMove

-- Evaluates the action on content
performPlayerContentEvent :: AppState -> LangL ()
performPlayerContentEvent st = do
  pos <- getPlayerPos st
  (_, content) <- getCell st pos
  performPlayerContentEvent' st pos content

performPlayerContentEvent' :: AppState -> Pos -> Content -> LangL ()
performPlayerContentEvent' _ _ NoContent = pure ()
performPlayerContentEvent' st pos Treasure = do
  addGameMessage st "You found a treasure!"
  writeVarIO (st ^. playerInventory . treasureState) True
  setCellContent st pos NoContent
performPlayerContentEvent' st _ (Wormhole n) = do
  addGameMessage st $ "You found a wormhole. You have been moved to the next wormhole."
  executeWormhole st n

addGameMessage :: AppState -> String -> LangL ()
addGameMessage st msg = do
  msgs <- readVarIO $ st ^. gameMessages
  writeVarIO (st ^. gameMessages) $ msgs ++ [msg]

makePlayerMove :: AppState -> Direction -> LangL ()
makePlayerMove st dir = do
  cancelPlayerLeaving st

  plPos       <- readVarIO $ st ^. playerPos
  lab         <- readVarIO $ st ^. labyrinth

  case testMove plPos dir lab of
    InvalidMove msg        -> throwException $ InvalidOperation msg
    ImpossibleMove msg     -> addGameMessage st msg
    LeavingLabyrinthMove   -> setGameState st PlayerIsAboutLeaving
    SuccessfullMove newPos -> do
      addGameMessage st "Step executed."
      setPlayerPos st newPos
      performPlayerContentEvent st

evalSkip :: AppState -> LangL ()
evalSkip st = do
  cancelPlayerLeaving st
  performPlayerContentEvent st

quit :: AppState -> LangL ()
quit st = setGameState st GameFinished

handleYes :: AppState -> LangL ()
handleYes st = do
  gameSt <- getGameState st
  case gameSt of
    PlayerIsAboutLossLeavingConfirmation -> do
      addGameMessage st loosing
      setGameState st GameFinished
    PlayerIsAboutLeaving -> throwException $ InvalidOperation "handleYes: Invalid state: PlayerIsAboutLeaving"
    _ -> addGameMessage st $ unknownCommand "yes"

handleNo :: AppState -> LangL ()
handleNo st = do
  gameSt <- getGameState st
  case gameSt of
    PlayerIsAboutLeaving -> throwException $ InvalidOperation "handleNo: Invalid state: PlayerIsAboutLeaving"
    PlayerIsAboutLossLeavingConfirmation -> cancelPlayerLeaving st
    _ -> addGameMessage st $ unknownCommand "no"

printLab :: AppState -> LangL ()
printLab st = do
  lab        <- readVarIO $ st ^. labyrinth
  plPos      <- readVarIO $ st ^. playerPos
  brPos      <- readVarIO $ st ^. bearPos
  template   <- readVarIO $ st ^. labRenderTemplate

  printLabRender' $ renderLabyrinth' template lab plPos brPos

moveBear :: AppState -> LangL ()
moveBear st = do
  dir   <- getRandomDirection
  brPos <- readVarIO $ st ^. bearPos
  let newPos = calcNextPos brPos dir
  bounds <- readVarIO $ st ^. labBounds
  lab    <- readVarIO $ st ^. labyrinth
  when (inBounds bounds newPos) $ do
    case Map.lookup brPos lab of
      Nothing        -> throwException $ InvalidOperation $ "Bear in on invalid position: " <> show brPos
      Just (cell, _) -> unless (isWallOnDirection cell dir) $
        writeVarIO (st ^. bearPos) newPos

onStep :: AppState -> () -> AppL D.CliAction
onStep st _ = do
  gameSt   <- scenario $ getGameState st
  treasure <- scenario $ getPlayerThreasureState st
  isFinished <- scenario $ case gameSt of
    PlayerIsAboutLeaving -> do
      when treasure $ addGameMessage st winning
      when treasure $ setGameState st GameFinished

      unless treasure $ addGameMessage st leaveWithoutTreasure
      unless treasure $ setGameState st PlayerIsAboutLossLeavingConfirmation

      pure treasure
    PlayerIsAboutLossLeavingConfirmation ->
      throwException $ InvalidOperation "OnStep: Invalid state: PlayerIsAboutLossLeavingConfirmation"
    PlayerMove   -> do
      moveBear st
      pure False
    GameFinished -> pure True
    GameStart    -> pure False

  msgs <- readVarIO $ st ^. gameMessages
  writeVarIO (st ^. gameMessages) []
  let outputMsg = intercalate "\n" msgs

  case isFinished of
    True  | null outputMsg -> pure $ D.CliFinish $ Just "Bye-bye"
          | otherwise      -> pure $ D.CliFinish $ Just $ outputMsg <> "\n" <> "Bye-bye"
    False | null outputMsg -> pure D.CliLoop
          | otherwise      -> pure $ D.CliOutputMsg outputMsg

startGame :: AppState -> Int -> AppL String
startGame st s = scenario $ generateLabyrinth (s, s) 3 5 >>= startGame' st

saveGame :: AppState -> Int -> AppL String
saveGame st idx = do
  -- TODO: do not save game when it's not a player move
  lab   <- readVarIO $ st ^. labyrinth
  plPos <- readVarIO $ st ^. playerPos
  plHP  <- readVarIO $ st ^. playerHP
  tr    <- readVarIO $ st ^. playerInventory . treasureState
  brPos <- readVarIO $ st ^. bearPos

  let plInv = Inventory tr

  eRes <- KVDB.saveGameState (st ^. kvdbConfig)
    $ KVDB.GameEntity idx lab plPos plHP plInv brPos

  case eRes of
    Left err -> pure $ show err
    Right _ -> pure "Game successully saved to KV DB."

loadGame :: AppState -> Int -> AppL String
loadGame st idx = do
  eRes <- KVDB.loadGameState (st ^. kvdbConfig) idx
  case eRes of
    Left err  -> pure $ "Failed to load game state from KV DB: " <> show err
    Right (KVDB.GameEntity {..}) -> do
      let LabyrinthInfo {liBounds, liWormholes} = analyzeLabyrinth geLab
      let renderTemplate = renderSkeleton liBounds
      writeVarIO (st ^. labBounds) liBounds
      writeVarIO (st ^. labRenderTemplate) renderTemplate
      writeVarIO (st ^. labRenderVar) renderTemplate
      writeVarIO (st ^. labWormholes) liWormholes
      writeVarIO (st ^. labyrinth) geLab
      writeVarIO (st ^. playerPos) gePlayerPos
      writeVarIO (st ^. playerHP) gePlayerHP
      writeVarIO (st ^. playerInventory . treasureState) $ treasureFound gePlayerInventory
      writeVarIO (st ^. bearPos) geBearPos
      writeVarIO (st ^. gameState) PlayerMove
      pure "Game succesfully loaded from KV DB."

startRndGame :: AppState -> LangL ()
startRndGame st = do
  lab <- generateRndLabyrinth
  msg <- startGame' st lab
  addGameMessage st msg

startGame' :: AppState -> Labyrinth -> LangL String
startGame' st lab = do
  let LabyrinthInfo {liBounds, liWormholes} = analyzeLabyrinth lab
  let (xSize, ySize) = liBounds
  let renderTemplate = renderSkeleton liBounds

  playerX <- getRandomInt (0, xSize - 1)
  playerY <- getRandomInt (0, ySize - 1)
  bearX   <- getRandomInt (0, xSize - 1)
  bearY   <- getRandomInt (0, ySize - 1)

  writeVarIO (st ^. labyrinth) lab
  writeVarIO (st ^. labBounds) liBounds
  writeVarIO (st ^. labRenderTemplate) renderTemplate
  writeVarIO (st ^. labRenderVar) renderTemplate
  writeVarIO (st ^. labWormholes) liWormholes
  writeVarIO (st ^. playerPos) (playerX, playerY)
  writeVarIO (st ^. playerHP) 100
  writeVarIO (st ^. bearPos) (bearX, bearY)
  writeVarIO (st ^. playerInventory . treasureState) False
  writeVarIO (st ^. gameState) PlayerMove

  pure "New game started."


onUnknownCommand :: AppState -> String -> AppL CliAction
onUnknownCommand _ ""     = pure D.CliLoop
onUnknownCommand st cmdStr = do
  case Str.words cmdStr of
    ["start", sizeStr] -> case readMaybe sizeStr of
      Nothing   -> pure $ D.CliOutputMsg $ "start command should have 1 int argument."
      Just size -> D.CliOutputMsg <$> startGame st size
    ["save", idxStr] -> case readMaybe idxStr of
      Nothing  -> pure $ D.CliOutputMsg $ "save command should have 1 int argument."
      Just idx -> D.CliOutputMsg <$> saveGame st idx
    ["load", idxStr] -> case readMaybe idxStr of
      Nothing  -> pure $ D.CliOutputMsg $ "load command should have 1 int argument."
      Just idx -> D.CliOutputMsg <$> loadGame st idx
    _ -> pure $ D.CliOutputMsg $ unknownCommand cmdStr

onPlayerMove :: AppState -> LangL () -> LangL ()
onPlayerMove st act = do
  gs <- getGameState st
  case gs of
    GameFinished -> addGameMessage st "Game finished. Please type 'start <lab_size>' to start a new game."
    GameStart -> addGameMessage st "Game is not yet started. Please type 'start <lab_size>' to start a new game."
    _ -> act

help :: LangL ()
help = do
   putStrLn "COMMAND OPTIONS"
   putStrLn " go up OR up:        player moves a space up in the Labyrinth grid"
   putStrLn " go down OR down:    player moves a space down in the Labyrinth grid"
   putStrLn " go left OR left:    player moves a space to the left in the Labyrinth grid"
   putStrLn " go right OR right:  player moves a space to the right in the Labyrinth grid"

   putStrLn " yes:                answer 'yes' to the question being asked"
   putStrLn " no:                 answer 'no' to the question being asked"

   putStrLn " skip:               player moves no where and stays in the current position"

   putStrLn " start:              begin the game with a random Labyrinth size"
   putStrLn " start (n):          begin the game with a Labyrinth of size (n x n)"

   putStrLn " quit OR q:          quit the game"

   putStrLn " print:              show the current state of the player in the Labyrinth"

   putStrLn "  "
   putStrLn "LABYRINTH SYMBOLS"
   putStrLn " @:                  player"
   putStrLn " W(n):               worm hole that first tries to direct player to W(n+1), then to W0"
   putStrLn " T:                  treasure"
   putStrLn " B:                  bear"



initAppState
  :: PlayerHasTreasure
  -> PlayerPos
  -> PlayerHP
  -> BearPos
  -> Labyrinth
  -> GameState
  -> KVDBConfig KVDB.LabKVDB
  -> AppL AppState
initAppState tr plPos plHP brPos lab gst kvdbCfg = do
  let LabyrinthInfo {liBounds, liWormholes} = analyzeLabyrinth lab
  let renderTemplate = renderSkeleton liBounds

  renderTemplateVar <- newVarIO renderTemplate
  renderVar         <- newVarIO renderTemplate
  labVar            <- newVarIO lab
  labBoundsVar      <- newVarIO liBounds
  wormholesVar      <- newVarIO liWormholes
  posVar            <- newVarIO plPos
  playerHPVar       <- newVarIO plHP
  bearPosVar        <- newVarIO brPos
  inv               <- InventoryState <$> newVarIO tr
  gameStateVar      <- newVarIO gst
  moveMsgsVar       <- newVarIO []

  pure $ AppState
    labVar
    labBoundsVar
    renderTemplateVar
    renderVar
    wormholesVar
    posVar
    playerHPVar
    bearPosVar
    inv
    gameStateVar
    moveMsgsVar
    kvdbCfg

labyrinthApp :: AppState -> AppL ()
labyrinthApp st = do
  scenario $ putStrLn "Labyrinth (aka Terra Incognita) game"
  scenario $ putStrLn "start <lab_size>   to start a new game"
  scenario $ putStrLn "load-kvdb <idx>    to load a game from KV DB (file by default)"
  scenario $ putStrLn "save-kvdb <idx>    to save a game to KV DB (file by default)"

  cliToken <- cli (onStep st) (onUnknownCommand st) $ do
    cmd "help"     $ help
    cmd "go up"    $ onPlayerMove st $ makeMove st DirUp
    cmd "go down"  $ onPlayerMove st $ makeMove st DirDown
    cmd "go left"  $ onPlayerMove st $ makeMove st DirLeft
    cmd "go right" $ onPlayerMove st $ makeMove st DirRight

    cmd "up"       $ onPlayerMove st $ makePlayerMove st DirUp
    cmd "down"     $ onPlayerMove st $ makePlayerMove st DirDown
    cmd "left"     $ onPlayerMove st $ makePlayerMove st DirLeft
    cmd "right"    $ onPlayerMove st $ makePlayerMove st DirRight

    cmd "yes"      $ handleYes st
    cmd "no"       $ handleNo st

    cmd "skip"     $ onPlayerMove st $ evalSkip st

    cmd "start"    $ startRndGame st

    cmd "quit"     $ quit st
    cmd "q"        $ quit st

    cmd "print"    $ printLab st

  atomically $ do
    finished <- readVar $ D.cliFinishedToken cliToken
    when (not finished) retry
