module Labyrinth.App where

import qualified Data.Text     as T
import qualified Data.Map      as Map

import Labyrinth.Prelude       as L
import Labyrinth.Domain
import Labyrinth.Types

-- |---|
-- | ? |
-- |---|
-- | ^ |
-- |---|

data Direction = Up | Down | Left | Right

data Passage
  = Passage
  | Exit
  | MonolithWall
  | RegularWall


getPassage :: Cell -> Direction -> Passage
getPassage (Cell _ _ NoWall _) Up    = Passage
getPassage (Cell _ _ _ NoWall) Down  = Passage
getPassage (Cell NoWall _ _ _) Left  = Passage
getPassage (Cell _ NoWall _ _) Right = Passage

getPassage (Cell _ _ (Monolith True) _) Up    = Exit
getPassage (Cell _ _ _ (Monolith True)) Down  = Exit
getPassage (Cell (Monolith True) _ _ _) Left  = Exit
getPassage (Cell _ (Monolith True) _ _) Right = Exit

getPassage (Cell _ _ (Monolith False) _) Up    = MonolithWall
getPassage (Cell _ _ _ (Monolith False)) Down  = MonolithWall
getPassage (Cell (Monolith False) _ _ _) Left  = MonolithWall
getPassage (Cell _ (Monolith False) _ _) Right = MonolithWall

getPassage _ _ = RegularWall

setPlayerLeavingField :: StateVar (Bool, HasATreasure) -> Bool -> LangL ()
setPlayerLeavingField var hasATreasure = atomically $ writeVar var (True, hasATreasure)

calcNextPos :: (Int, Int) -> Direction -> (Int, Int)
calcNextPos (x, y) Up    = (x, y - 1)
calcNextPos (x, y) Down  = (x, y + 1)
calcNextPos (x, y) Left  = (x - 1, y)
calcNextPos (x, y) Right = (x + 1, y)

data MovingResult
  = MoveSuccessfull (Maybe Content)
  | MoveImpossibleMonolith
  | MoveImpossibleRegularWall
  | EndMove Bool
  | InvalidMove Text

testMove :: GameState -> Direction -> LangL MovingResult
testMove st dir = do
  lab         <- atomically $ readVar $ _labyrinth st
  hasTreasure <- atomically $ readVar $ _treasure $ _inventory st
  curPos      <- atomically $ readVar $ _playerPos st
  let nextPos = calcNextPos curPos dir

  pure $ case Map.lookup curPos lab of
    Nothing        -> InvalidMove $ "No cell found: " +|| prevPos ||+ ""
    Just (cell, _) -> case getPassage cell dir of
      MonolithWall -> MoveImpossibleMonolith
      RegularWall -> MoveImpossibleRegularWall

      Passage -> case Map.lookup nextPos lab of
        Nothing -> InvalidMove $ "No cell found: " +|| nextPos ||+ ""
        Just (nextCell, content) -> MoveSuccessfull $ Just content

      Exit -> EndMove hasTreasure


        -- putStrLn "You didn't find a treasure. Leaving means failure. Do you want to leave the labyrinth?"
        -- putStrLn "Congratulations! You won!"

goUp :: GameState -> LangL (Maybe String)
goUp st = do
  mbContent <- testMove st Up ()


quit :: GameState -> LangL (Maybe String)
quit st = do
  -- Do something
  throwException $ Finished False

mainLoop :: GameState -> AppL ()
mainLoop st = std $ do
  simpleCmd_ "go up" $ goUp st

  simpleCmd_ "quit" $ quit st

app :: GameState -> AppL ()
app st = do
  scenario $ putStrLn "Labyrinth (aka Terra Incognita) game"

  forever mainLoop
