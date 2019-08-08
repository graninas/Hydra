{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Astro.Lens where

import           Control.Lens       (makeFieldsNoPrefix)
import           Astro.Types (Coords, Meteor, AppState)

makeFieldsNoPrefix ''Coords
makeFieldsNoPrefix ''Meteor
makeFieldsNoPrefix ''AppState
