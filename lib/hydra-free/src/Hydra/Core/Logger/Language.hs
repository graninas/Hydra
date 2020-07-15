{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module Hydra.Core.Logger.Language where

import           Hydra.Prelude

import qualified Hydra.Core.Domain               as D

import Hydra.Core.Logger.Class


-- | Language for logging.
data LoggerF next where
  -- | Log message with a predefined level.
  LogMessage :: !D.LogLevel -> !D.Message -> (() -> next) -> LoggerF next

instance Functor LoggerF where
  fmap f (LogMessage lvl msg next) = LogMessage lvl msg (f . next)


type LoggerL = Free LoggerF

instance Logger LoggerL where
  logMessage level msg = liftF $ LogMessage level msg id
