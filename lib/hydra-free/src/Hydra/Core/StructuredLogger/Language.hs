{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module Hydra.Core.StructuredLogger.Language where

import           Hydra.Prelude

import qualified Hydra.Core.Domain as D
import qualified Hydra.Core.StructuredLogger.Class as C


-- | Language for structured logging.
data StructuredLoggerF next where
  -- | Report message to a specific service such as Sentry.
  Report
    :: !D.LogLevel
    -> !D.Message
    -> !(Maybe UTCTime)
    -> ![D.Attribute]
    -> (() -> next)
    -> StructuredLoggerF next


instance Functor StructuredLoggerF where
  fmap f (Report lvl msg mbTimestamp attrs next) =
    Report lvl msg mbTimestamp attrs (f . next)


type StructuredLoggerL = Free StructuredLoggerF
type SLogger = StructuredLoggerL

instance C.StructuredLogger StructuredLoggerL where
  report level msg mbTimestamp attrs =
    liftF $ Report level msg mbTimestamp attrs id
