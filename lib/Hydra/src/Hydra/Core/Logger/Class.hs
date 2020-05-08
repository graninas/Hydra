{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module Hydra.Core.Logger.Class where

import           Hydra.Prelude

import qualified Hydra.Core.Domain as D

class Monad m => Logger m where
  logMessage :: D.LogLevel -> D.Message -> m ()

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
