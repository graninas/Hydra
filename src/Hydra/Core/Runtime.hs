module Hydra.Core.Runtime where

import           Hydra.Prelude

import qualified Data.Map                        as Map

import qualified Hydra.Core.Domain               as D
import qualified Hydra.Core.Language             as L
import qualified Hydra.Core.Logger.Impl.HsLogger as Impl

-- | Runtime data for the concrete logger impl.
newtype LoggerRuntime = LoggerRuntime
    { _hsLoggerHandle :: Maybe Impl.HsLoggerHandle
    }

data ProcessRuntime = ProcessRuntime
    { _idCounter :: IORef Int
    , _processes :: TVar (Map D.ProcessId ThreadId)
    }

-- | Runtime data for core subsystems.
data CoreRuntime = CoreRuntime
    { _loggerRuntime  :: LoggerRuntime
    , _stateRuntime   :: StateRuntime
    }

-- | Logger that can be used in runtime via the logging subsystem.
newtype RuntimeLogger = RuntimeLogger
    { logMessage' :: D.LogLevel -> D.Message -> IO ()
    }

newtype VarHandle = VarHandle (TVar Any)

data StateRuntime = StateRuntime
    { _varId  :: TVar D.VarId                       -- ^ Var id counter
    , _state  :: TMVar (Map.Map D.VarId VarHandle)  -- ^ Node state.
    , _stmLog :: TVar D.Log                         -- ^ Stm log entries
    }

createVoidLoggerRuntime :: IO LoggerRuntime
createVoidLoggerRuntime = pure $ LoggerRuntime Nothing

createLoggerRuntime :: D.LoggerConfig -> IO LoggerRuntime
createLoggerRuntime config = LoggerRuntime . Just <$> Impl.setupLogger config

clearLoggerRuntime :: LoggerRuntime -> IO ()
clearLoggerRuntime (LoggerRuntime (Just hsLogger)) = Impl.teardownLogger hsLogger
clearLoggerRuntime _                               = pure ()

createStateRuntime :: IO StateRuntime
createStateRuntime = StateRuntime
    <$> newTVarIO 0
    <*> newTMVarIO Map.empty
    <*> newTVarIO []

createProcessRuntime :: IO ProcessRuntime
createProcessRuntime = ProcessRuntime
    <$> newIORef 0
    <*> newTVarIO Map.empty

createCoreRuntime :: LoggerRuntime -> IO CoreRuntime
createCoreRuntime loggerRt = CoreRuntime
    <$> pure loggerRt
    <*> createStateRuntime

-- clearCoreRuntime :: CoreRuntime -> IO ()
-- clearCoreRuntime _ = pure ()

-- mkRuntimeLogger :: LoggerRuntime -> RuntimeLogger
-- mkRuntimeLogger (LoggerRuntime hsLog) = RuntimeLogger
--     { logMessage' = \lvl msg -> Impl.runLoggerL hsLog $ L.logMessage lvl msg
--     }

-- Runtime log functions
-- logInfo' :: RuntimeLogger -> D.Message -> IO ()
-- logInfo' (RuntimeLogger l) = l D.Info
--
-- logError' :: RuntimeLogger -> D.Message -> IO ()
-- logError' (RuntimeLogger l) = l D.Error
--
-- logDebug' :: RuntimeLogger -> D.Message -> IO ()
-- logDebug' (RuntimeLogger l) = l D.Debug
--
-- logWarning' :: RuntimeLogger -> D.Message -> IO ()
-- logWarning' (RuntimeLogger l) = l D.Warning


-- | Writes all stm entries into real logger.
flushStmLogger :: StateRuntime -> LoggerRuntime -> IO ()
flushStmLogger stateRt loggerRt = do
    l <- atomically $ do
            l <- readTVar $ _stmLog stateRt
            writeTVar (_stmLog stateRt) []
            pure l
    let loggerHandle = _hsLoggerHandle loggerRt
    mapM_ (\(D.LogEntry level msg) -> Impl.runLoggerL loggerHandle $ L.logMessage level msg) l
