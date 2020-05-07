module Labyrinth.App where

import qualified Data.Text     as T
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
  = SuccessfullMove ((Int, Int), (Cell, Content))
  | ImpossibleMove String
  | EndMove Bool
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
unknownCommand cmd = "Unknown command: " <> cmd

calcNextPos :: (Int, Int) -> Direction -> (Int, Int)
calcNextPos (x, y) DirUp    = (x, y - 1)
calcNextPos (x, y) DirDown  = (x, y + 1)
calcNextPos (x, y) DirLeft  = (x - 1, y)
calcNextPos (x, y) DirRight = (x + 1, y)

-- Testing the move. This function doesn't change game state.
testMove :: GameState -> Direction -> LangL MovingResult
testMove st dir = do
  lab         <- atomically $ readVar $ st ^. labyrinth
  hasTreasure <- atomically $ readVar $ st ^. playerInventory . treasure
  curPos      <- atomically $ readVar $ st ^. playerPos
  let nextPos    = calcNextPos curPos dir
  let mbCurCell  = Map.lookup curPos lab
  let mbNextCell = Map.lookup nextPos lab

  pure $ case mbCurCell of
    Nothing        -> InvalidMove $ "cell not found: " +|| curPos ||+ ""
    Just (cell, _) -> case (getPassage cell dir, mbNextCell) of
      (MonolithWall, _)        -> ImpossibleMove "step impossible: monolith wall"
      (RegularWall, _)         -> ImpossibleMove "step impossible: wall"
      (Passage, Nothing)       -> InvalidMove $ "cell not found: " +|| nextPos ||+ ""
      (Passage, Just nextCell) -> SuccessfullMove (nextPos, nextCell)
      (Exit, _)                -> EndMove hasTreasure
      m                        -> InvalidMove $ "invalid move: " +|| m ||+ ""

setPlayerPos :: GameState -> Pos -> LangL ()
setPlayerPos st newPos = atomically $ writeVar (st ^. playerPos) newPos

setCellContent :: GameState -> Pos -> Content -> LangL ()
setCellContent st pos content = do
  lab <- atomically $ readVar $ _labyrinth st
  case Map.lookup pos lab of
    Nothing        -> throwException $ InvalidOperation $ "cell not found: " +|| pos ||+ ""
    Just (cell, _) -> atomically$ writeVar (st ^. labyrinth) $ Map.insert pos (cell, content) lab

nextWormhole :: GameState -> Int -> Int
nextWormhole st n | (n + 1 < Map.size (st ^. wormholes)) = n + 1
                  | otherwise = 0

executeWormhole :: GameState -> Int -> LangL ()
executeWormhole st (nextWormhole st -> n) = case Map.lookup n (st ^. wormholes)  of
  Nothing  -> throwException $ InvalidOperation $ "wormhole not found: " +|| n ||+ ""
  Just pos -> setPlayerPos st pos

-- Evaluates the action on content
performContentEvent :: GameState -> Pos -> Content -> LangL ()
performContentEvent st _ NoContent = pure ()
performContentEvent st pos Treasure = do
  addMoveMessage st "you found a treasure!"
  atomically $ writeVar (st ^. playerInventory . treasure) True
  setCellContent st pos NoContent
performContentEvent st _ (Wormhole n) = do
  addMoveMessage st $ "you found a wormhole " +|| n ||+ ". You have been moved to the next wormhole."
  executeWormhole st n

clearPlayerLeaving :: GameState -> LangL ()
clearPlayerLeaving st = writeVarIO (st ^. playerIsAboutLeaving) Nothing

addMoveMessage :: GameState -> String -> LangL ()
addMoveMessage st msg = do
  msgs <- readVarIO $ st ^. moveMessages
  writeVarIO (st ^. moveMessages) $ msgs ++ [msg]

makeMove :: GameState -> Direction -> LangL ()
makeMove st dir = do
  clearPlayerLeaving st
  moveResult <- testMove st dir
  case moveResult of
    InvalidMove msg     -> throwException $ InvalidOperation msg
    ImpossibleMove msg  -> addMoveMessage st msg
    EndMove hasTreasure -> do
      if hasTreasure
        then addMoveMessage st winning
        else addMoveMessage st leaveWithoutTreasure
      atomically $ writeVar (st ^. playerIsAboutLeaving) (Just hasTreasure)
    SuccessfullMove (newPos, (_, content)) -> do
      addMoveMessage st "step executed."
      setPlayerPos st newPos
      performContentEvent st newPos content

quit :: GameState -> LangL ()
quit st = writeVarIO (st ^. gameFinished) True

handleYes :: GameState -> LangL ()
handleYes st = do
  isLeaving <- readVarIO $ st ^. playerIsAboutLeaving
  writeVarIO (st ^. playerIsAboutLeaving) Nothing
  case isLeaving of
    Nothing   -> addMoveMessage st $ unknownCommand "yes"
    Just True -> do
      putStrLn winning
      writeVarIO (st ^. gameFinished) True
    Just False -> do
      putStrLn loosing
      writeVarIO (st ^. gameFinished) True

handleNo :: GameState -> LangL ()
handleNo st = do
  isLeaving <- readVarIO $ st ^. playerIsAboutLeaving
  writeVarIO (st ^. playerIsAboutLeaving) Nothing
  case isLeaving of
    Nothing   -> addMoveMessage st $ unknownCommand "no"
    _ -> pure ()

printLabyrinth :: GameState -> LangL ()
printLabyrinth st = do
  lab               <- readVarIO $ st ^. labyrinth
  bounds            <- readVarIO $ st ^. labyrinthSize
  plPos             <- readVarIO $ st ^. playerPos
  let template = st ^. labRenderTemplate

  printLabRender bounds $ renderLabyrinth template lab plPos

onStep :: GameState -> () -> AppL D.CliAction
onStep st _ = do
  msgs <- readVarIO $ st ^. moveMessages
  writeVarIO (st ^. moveMessages) []
  let outputMsg = intercalate "\n" msgs

  finished <- readVarIO $ st ^. gameFinished
  case finished of
    True  -> pure $ D.CliFinish $ Just "Bye-bye"
    False -> pure $ D.CliOutputMsg outputMsg

onUnknownCommand :: String -> AppL CliAction
onUnknownCommand cmd = pure $ D.CliOutputMsg $ unknownCommand cmd

app :: GameState -> AppL ()
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

    cmd "quit"     $ quit st
    cmd "q"        $ quit st

    cmd "print"    $ printLabyrinth st

  atomically $ do
    finished <- readVar $ D.cliFinishedToken cliToken
    when (not finished) retry
