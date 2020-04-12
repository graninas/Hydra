module Labyrinth.Gen where

import qualified Data.Text     as T
import qualified Data.Map      as Map

import Labyrinth.Prelude       as L
import Labyrinth.Domain
import Labyrinth.Types

generateLabyrinth :: LangL Labyrinth
generateLabyrinth = throwException $ NotImplemented "Generation"
