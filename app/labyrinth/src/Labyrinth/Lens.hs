{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Labyrinth.Lens where

import           Control.Lens    (makeFieldsNoPrefix)
import           Labyrinth.Types (InventoryState, AppState)


makeFieldsNoPrefix ''AppState
makeFieldsNoPrefix ''InventoryState
