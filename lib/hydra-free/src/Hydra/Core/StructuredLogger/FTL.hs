{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module Hydra.Core.StructuredLogger.FTL where

import           Hydra.Prelude

import qualified Hydra.Core.Domain as D
import qualified Data.Time.Clock as Time


class Monad m => StructuredLoggerL m where
  report
    :: D.LogLevel
    -> D.Message
    -> Maybe Time.UTCTime
    -> [D.Attribute]
    -> m ()

