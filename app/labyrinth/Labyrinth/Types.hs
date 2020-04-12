module Labyrinth.Types where

import           Hydra.Prelude
import qualified Data.Text                  as T
import qualified Data.Map                   as Map

import qualified Hydra.Domain               as D
import qualified Hydra.Language             as L

import           Labyrinth.Domain

data GameState = GameState
  { _labyrinth :: Labyrinth
  }


type App = ReaderT GameState AppL
