module Labyrinth.App where

import qualified Data.Text     as T
import qualified Data.Map      as Map

import Labyrinth.Prelude       as L
import Labyrinth.Domain
import Labyrinth.Types

goUp :: GameState -> LangL (Maybe String)
goUp st = throwException $ NotImplemented "Go up command"

mainLoop :: GameState -> AppL ()
mainLoop st = std $ do
  simpleCmd_ "go up" $ goUp st

app :: GameState -> AppL ()
app st = do
  scenario $ putStrLn "Labyrinth (aka Terra Incognita) game"
