{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module Hydra.Core.Logger.Language where

import           Hydra.Prelude

import qualified Hydra.Core.Domain       as D
import qualified Hydra.Core.Logger.Class as C

-- | Language for logging.
data LoggerF next where
  -- | Log message with a predefined level.
  LogMessage :: !D.LogLevel -> !D.Message -> (() -> next) -> LoggerF next

instance Functor LoggerF where
  fmap f (LogMessage lvl msg next) = LogMessage lvl msg (f . next)

type LoggerL = F LoggerF

instance C.Logger LoggerL where
  logMessage level msg = liftFC $ LogMessage level msg id
