module Labyrinth.App where

import qualified Data.Map      as Map

import qualified Hydra.Domain  as D
import Labyrinth.Prelude       as L
import Labyrinth.Domain
import Labyrinth.Types
import Labyrinth.Render
import Labyrinth.Lens

data Passage
  = Passage
  | Exit
  | MonolithWall
  | RegularWall
  deriving (Show, Read, Eq)

data MovingResult
  = SuccessfullMove Pos Cell Content
  | ImpossibleMove String
  | ExitFound Bool
  | InvalidMove String
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

calcNextPos :: (Int, Int) -> Direction -> (Int, Int)
calcNextPos (x, y) DirUp    = (x, y - 1)
calcNextPos (x, y) DirDown  = (x, y + 1)
calcNextPos (x, y) DirLeft  = (x - 1, y)
calcNextPos (x, y) DirRight = (x + 1, y)

-- Testing the move. This function doesn't change game state.
testMove :: AppState -> Direction -> LangL MovingResult
testMove st dir = do
  lab         <- readVarIO $ st ^. labyrinth
  hasTreasure <- readVarIO $ st ^. playerInventory . treasure
  curPos      <- readVarIO $ st ^. playerPos
  let nextPos    = calcNextPos curPos dir
  let mbCurCell  = Map.lookup curPos lab
  let mbNextCell = Map.lookup nextPos lab

  pure $ case mbCurCell of
    Nothing        -> InvalidMove $ "Cell not found: " +|| curPos ||+ ""
    Just (cell, _) -> case (getPassage cell dir, mbNextCell) of
      (MonolithWall, _)        -> ImpossibleMove "Step impossible: monolith wall"
      (RegularWall, _)         -> ImpossibleMove "Step impossible: wall"
      (Passage, Nothing)       -> InvalidMove $ "Cell not found: " +|| nextPos ||+ ""
      (Passage, Just (nextCell, nextContent)) -> SuccessfullMove nextPos nextCell nextContent
      (Exit, _)                -> ExitFound hasTreasure

getPlayerPos :: AppState -> LangL Pos
getPlayerPos st = readVarIO $ st ^. playerPos

setPlayerPos :: AppState -> Pos -> LangL ()
setPlayerPos st newPos = writeVarIO (st ^. playerPos) newPos

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

nextWormhole :: AppState -> Int -> Int
nextWormhole st n | (n + 1 < Map.size (st ^. wormholes)) = n + 1
                  | otherwise = 0

executeWormhole :: AppState -> Int -> LangL ()
executeWormhole st (nextWormhole st -> n) = case Map.lookup n (st ^. wormholes)  of
  Nothing  -> throwException $ InvalidOperation $ "Wormhole not found: " +|| n ||+ ""
  Just pos -> setPlayerPos st pos

cancelPlayerLeaving :: AppState -> LangL ()
cancelPlayerLeaving st = do
  gameSt <- getGameState st
  case gameSt of
    PlayerIsAboutLeaving _               -> addMoveMessage st "Okay, continue."
    PlayerIsAboutLossLeavingConfirmation -> addMoveMessage st "Okay, continue."
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
  addMoveMessage st "You found a treasure!"
  writeVarIO (st ^. playerInventory . treasure) True
  setCellContent st pos NoContent
performPlayerContentEvent' st _ (Wormhole n) = do
  addMoveMessage st $ "You found a wormhole. You have been moved to the next wormhole."
  executeWormhole st n

addMoveMessage :: AppState -> String -> LangL ()
addMoveMessage st msg = do
  msgs <- readVarIO $ st ^. moveMessages
  writeVarIO (st ^. moveMessages) $ msgs ++ [msg]

makeMove :: AppState -> Direction -> LangL ()
makeMove st dir = do
  cancelPlayerLeaving st
  moveResult <- testMove st dir
  case moveResult of
    InvalidMove msg     -> throwException $ InvalidOperation msg
    ImpossibleMove msg  -> addMoveMessage st msg
    ExitFound hasTreasure -> setGameState st $ PlayerIsAboutLeaving hasTreasure
    SuccessfullMove newPos _ _ -> do
      addMoveMessage st "Step executed."
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
      addMoveMessage st loosing
      setGameState st GameFinished
    PlayerIsAboutLeaving _ -> throwException $ InvalidOperation "handleYes: Invalid state: PlayerIsAboutLeaving"
    _ -> addMoveMessage st $ unknownCommand "yes"

handleNo :: AppState -> LangL ()
handleNo st = do
  gameSt <- getGameState st
  case gameSt of
    PlayerIsAboutLeaving _ -> throwException $ InvalidOperation "handleNo: Invalid state: PlayerIsAboutLeaving"
    PlayerIsAboutLossLeavingConfirmation -> cancelPlayerLeaving st
    _ -> addMoveMessage st $ unknownCommand "no"

printLabyrinth :: AppState -> LangL ()
printLabyrinth st = do
  lab               <- readVarIO $ st ^. labyrinth
  bounds            <- readVarIO $ st ^. labyrinthSize
  plPos             <- readVarIO $ st ^. playerPos
  let template = st ^. labRenderTemplate

  printLabRender bounds $ renderLabyrinth template lab plPos

onStep :: AppState -> () -> AppL D.CliAction
onStep st _ = do
  gameSt <- scenario $ getGameState st
  isFinished <- scenario $ case gameSt of
    PlayerIsAboutLeaving True  -> do
      addMoveMessage st winning
      setGameState st GameFinished
      pure True
    PlayerIsAboutLeaving False -> do
      addMoveMessage st leaveWithoutTreasure
      setGameState st PlayerIsAboutLossLeavingConfirmation
      pure False
    PlayerIsAboutLossLeavingConfirmation ->
      throwException $ InvalidOperation "OnStep: Invalid state: PlayerIsAboutLossLeavingConfirmation"
    PlayerMove   -> pure False
    GameFinished -> pure True

  msgs <- readVarIO $ st ^. moveMessages
  writeVarIO (st ^. moveMessages) []
  let outputMsg = intercalate "\n" msgs

  case isFinished of
    True  | null outputMsg -> pure $ D.CliFinish $ Just "Bye-bye"
          | otherwise      -> pure $ D.CliFinish $ Just $ outputMsg <> "\n" <> "Bye-bye"
    False | null outputMsg -> pure D.CliLoop
          | otherwise      -> pure $ D.CliOutputMsg outputMsg

onUnknownCommand :: String -> AppL CliAction
onUnknownCommand ""     = pure D.CliLoop
onUnknownCommand cmdStr = pure $ D.CliOutputMsg $ unknownCommand cmdStr

app :: AppState -> AppL ()
app st = do
  scenario $ putStrLn "Labyrinth (aka Terra Incognita) game"

  cliToken <- cli (onStep st) onUnknownCommand $ do
    cmd "go up"    $ makeMove st DirUp
    cmd "go down"  $ makeMove st DirDown
    cmd "go left"  $ makeMove st DirLeft
    cmd "go right" $ makeMove st DirRight

    cmd "up"       $ makeMove st DirUp
    cmd "down"     $ makeMove st DirDown
    cmd "left"     $ makeMove st DirLeft
    cmd "right"    $ makeMove st DirRight

    cmd "yes"      $ handleYes st
    cmd "no"       $ handleNo st

    cmd "skip"     $ evalSkip st

    cmd "quit"     $ quit st
    cmd "q"        $ quit st

    cmd "print"    $ printLabyrinth st

  atomically $ do
    finished <- readVar $ D.cliFinishedToken cliToken
    when (not finished) retry
