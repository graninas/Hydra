{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Hydra.Testing.Functional.RLens where

import           Control.Lens (makeFieldsNoPrefix)
import           Hydra.Testing.Functional.TestRuntime (TestRuntime)

makeFieldsNoPrefix ''TestRuntime
