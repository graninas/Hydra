{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module Hydra.Core.Logger.FTL where

import           Hydra.Prelude

import qualified Hydra.Core.Domain as D


class Monad m => LoggerL m where
  logMessage :: D.LogLevel -> D.Message -> m ()


-- | Log message with Info level.
logInfo :: LoggerL m => D.Message -> m ()
logInfo = logMessage D.Info

-- | Log message with Error level.
logError :: LoggerL m => D.Message -> m ()
logError = logMessage D.Error

-- | Log message with Debug level.
logDebug :: LoggerL m => D.Message -> m ()
logDebug = logMessage D.Debug

-- | Log message with Warning level.
logWarning :: LoggerL m => D.Message -> m ()
logWarning = logMessage D.Warning
