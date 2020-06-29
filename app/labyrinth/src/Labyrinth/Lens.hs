{-|
" ^. "  -- implicit hidden behaviors of these -- goes through with compiler
-}


{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Labyrinth.Lens where

import           Control.Lens    (makeFieldsNoPrefix)
import           Labyrinth.Types (InventoryState, AppState)


makeFieldsNoPrefix ''AppState
makeFieldsNoPrefix ''InventoryState


-- uses _playerInventory
-- playerInventory :: Lens AppState InventoryState
-- playerInventory = ...



-- uses _theMapState
-- theMapState :: Lens InventoryState (StateVar PlayerHasTheMap)
-- theMapState = ...
