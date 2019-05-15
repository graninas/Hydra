module Hydra.Core.Logger.Impl.StmLoggerChurch where

import           Hydra.Prelude

import qualified Hydra.Core.ChurchL  as CL
import qualified Hydra.Core.Domain   as D
import qualified Hydra.Core.Language as L


-- | Interpret LoggerF language for a stm log.
interpretStmLoggerF :: TVar D.Log -> L.LoggerF a -> STM a
interpretStmLoggerF stmLog (L.LogMessage level msg next) =
    next <$> modifyTVar stmLog (D.LogEntry level msg :)

-- | Run LoggerL language for a stm log.
runStmLoggerL :: TVar D.Log -> CL.LoggerL () -> STM ()
runStmLoggerL stmLog = foldF (interpretStmLoggerF stmLog)
