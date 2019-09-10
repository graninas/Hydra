{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Hydra.Framework.RLens where

import           Control.Lens (makeFieldsNoPrefix)
import           Hydra.Framework.Runtime (AppRuntime)

makeFieldsNoPrefix ''AppRuntime
