{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Labyrinth.Lens where

import           Control.Lens    (makeFieldsNoPrefix)
import           Labyrinth.Types (Inventory, GameState)


makeFieldsNoPrefix ''GameState
makeFieldsNoPrefix ''Inventory
