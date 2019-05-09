{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module Hydra.Core.Logger.ChurchL where

import           Hydra.Prelude

import qualified Hydra.Core.Domain          as D
import qualified Hydra.Core.Logger.Language as L
import qualified Hydra.Core.Logger.Class    as L

type LoggerL = F L.LoggerF

instance L.Logger LoggerL where
  logMessage level msg = liftFC $ L.LogMessage level msg id
