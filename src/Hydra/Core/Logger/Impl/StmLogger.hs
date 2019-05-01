module Hydra.Core.Logger.Impl.StmLogger where

import           Hydra.Prelude

import qualified Hydra.Core.Domain   as D
import qualified Hydra.Core.Language as L


-- | Interpret LoggerF language for a stm log.
interpretStmLoggerF :: TVar D.Log -> L.LoggerF a -> STM a
interpretStmLoggerF stmLog (L.LogMessage level msg next) =
    next <$> modifyTVar stmLog (D.LogEntry level msg :)

-- | Run LoggerL language for a stm log.
runStmLoggerL :: TVar D.Log -> L.LoggerL () -> STM ()
runStmLoggerL stmLog = foldFree (interpretStmLoggerF stmLog)
