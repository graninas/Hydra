{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Hydra.Core.RLens where

import           Control.Lens       (makeFieldsNoPrefix)
import           Hydra.Core.Runtime (CoreRuntime, LoggerRuntime, ProcessRuntime, StateRuntime)

makeFieldsNoPrefix ''CoreRuntime
makeFieldsNoPrefix ''LoggerRuntime
makeFieldsNoPrefix ''StateRuntime
makeFieldsNoPrefix ''ProcessRuntime
