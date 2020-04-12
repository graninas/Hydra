module Labyrinth.App where

import           Hydra.Prelude
import qualified Data.Text                  as T
import qualified Data.Map                   as Map

import qualified Hydra.Domain               as D
import qualified Hydra.Language             as L


mainLoop :: AppL ()
mainLoop st = L.std $ do
  L.userCmd "go up" $ goUp st

app :: AppL ()
app = do
  L.scenario $ putStrLn "Labyrinth (aka Terra Incognita) game"
