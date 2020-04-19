module Labyrinth.App where

import qualified Data.Text     as T
import qualified Data.Map      as Map

import qualified Hydra.Domain  as D
import Labyrinth.Prelude       as L
import Labyrinth.Domain
import Labyrinth.Types
import Labyrinth.Lens

-- |---|
-- | ? |
-- |---|
-- | ^ |
-- |---|

data Direction = DirUp | DirDown | DirLeft | DirRight
  deriving (Show, Read, Eq)

data Passage
  = Passage
  | Exit
  | MonolithWall
  | RegularWall
  deriving (Show, Read, Eq)

data MovingResult
  = SuccessfullMove ((Int, Int), (Cell, Content))
  | ImpossibleMove Text
  | EndMove Bool
  | InvalidMove Text
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

playerLeaving :: GameState -> Bool -> LangL ()
playerLeaving st hasTreasure = do
  outputMsg
  atomically $ writeVar (st ^. playerIsAboutLeaving ) (Just hasTreasure)
  where
    outputMsg | hasTreasure = putStrLn "Congratulations! You win!"
              | otherwise   = putStrLn "You didn't find a treasure. Leaving means failure. Do you want to leave the labyrinth?"

calcNextPos :: (Int, Int) -> Direction -> (Int, Int)
calcNextPos (x, y) DirUp    = (x, y - 1)
calcNextPos (x, y) DirDown  = (x, y + 1)
calcNextPos (x, y) DirLeft  = (x - 1, y)
calcNextPos (x, y) DirRight = (x + 1, y)

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

performContentEvent :: GameState -> Pos -> Content -> LangL ()
performContentEvent st _ NoContent = pure ()
performContentEvent st pos Treasure = do
  putStrLn "you found a treasure!"
  atomically $ writeVar (st ^. playerInventory . treasure) True
  setCellContent st pos NoContent
performContentEvent st _ (Wormhole n) = do
  putStrLn $"you found a wormhole " +|| n ||+ ". You have been moved to the next wormhole."
  executeWormhole st n

makeMove :: GameState -> Direction -> LangL ()
makeMove st dir = do
  moveResult <- testMove st dir
  case moveResult of
    InvalidMove msg     -> throwException $ InvalidOperation msg
    ImpossibleMove msg  -> putStrLn msg
    EndMove hasTreasure -> playerLeaving st hasTreasure
    SuccessfullMove (newPos, (_, content)) -> do
      putStrLn "step executed."
      setPlayerPos st newPos
      performContentEvent st newPos content

quit :: GameState -> LangL ()
quit st = writeVarIO (st ^. gameFinished) True

printLabyrinth :: GameState -> LangL ()
printLabyrinth st = do
  lab          <- readVarIO $ st ^. labyrinth
  (maxX, maxY) <- readVarIO $ st ^. labyrinthSize

  let printAndMergeCells y x (t1, m1, b1) = case Map.lookup (x, y) lab of
        Nothing -> (t1 <> "!III!", m1 <> "!III!", b1 <> "!III!")
        Just (c, content) ->
          let (t,m,b) = printCell c content
          in (t1 <> t, m1 <> m, b1 <> b)

  let printAndMergeRows y rows =
        let row@(t, m, b) = foldr (printAndMergeCells y) ([],[],[]) [0..maxX-1]
        in rows ++ [row]

  let printedRows = foldr printAndMergeRows [] [0..maxY-1]

  let outputRows (t, m, b) = putStrLn $ T.pack $ t <> "\n" <> m <> "\n" <> b
  mapM_ outputRows printedRows



--
-- -   ------
-- |W1 ||W2P|
-- ------   -
-- ----------
-- |  T||   |
-- ----------
--

printCell (Cell l r u d) content
  = ( printHorizontalWall u
    , printVerticalWall l <> printContent content <> printVerticalWall r
    , printHorizontalWall d
    )

printHorizontalWall NoWall            = "-   -"
printHorizontalWall Wall              = "-----"
printHorizontalWall (Monolith True)   = "#   #"
printHorizontalWall (Monolith False)  = "#---#"

printVerticalWall NoWall           = " "
printVerticalWall Wall             = "|"
printVerticalWall (Monolith True)  = " "
printVerticalWall (Monolith False) = "#"

printContent NoContent    = "   "
printContent Treasure     = "  T"
printContent (Wormhole n) = "W" <> show n <> " "


-- (-Y)
-- |
-- V
-- (+Y)

-- (-X) --- (+X)

onStep :: GameState -> () -> AppL D.CliAction
onStep st _ = do
  finished <- readVarIO $ st ^. gameFinished
  case finished of
    True  -> pure $ D.CliFinish $ Just "Bye-bye"
    False -> pure D.CliLoop

onUnknownCommand :: String -> AppL CliAction
onUnknownCommand cmd = pure $ D.CliOutputMsg $ "Unknown command: " <> cmd

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

    cmd "quit"     $ quit st

    cmd "print"    $ printLabyrinth st

  atomically $ do
    finished <- readVar $ D.cliFinishedToken cliToken
    when (not finished) retry
