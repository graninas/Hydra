{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module Hydra.Core.Logger.Language where

import           Hydra.Prelude

import qualified Hydra.Core.Domain               as D (LogLevel (..), Message)

import           Language.Haskell.TH.MakeFunctor

-- | Language for logging.
data LoggerF next where
  -- | Log message with a predefined level.
  LogMessage :: D.LogLevel -> D.Message -> (() -> next) -> LoggerF next

makeFunctorInstance ''LoggerF

type LoggerL = Free LoggerF

class Logger m where
  logMessage :: D.LogLevel -> D.Message -> m ()

instance Logger LoggerL where
  logMessage level msg = liftF $ LogMessage level msg id

-- | Log message with Info level.
logInfo :: Logger m => D.Message -> m ()
logInfo = logMessage D.Info

-- | Log message with Error level.
logError :: Logger m => D.Message -> m ()
logError = logMessage D.Error

-- | Log message with Debug level.
logDebug :: Logger m => D.Message -> m ()
logDebug = logMessage D.Debug

-- | Log message with Warning level.
logWarning :: Logger m => D.Message -> m ()
logWarning = logMessage D.Warning
