module Hydra.Core.Runtime where

import           Hydra.Prelude

import qualified Data.Map                        as Map

import qualified Hydra.Core.Domain               as D
import qualified Hydra.Core.Language             as L
import qualified Hydra.Core.Logger.Impl.HsLogger as Impl
import qualified Hydra.Core.Logger.Impl.HsLoggerInterpreter as I

import qualified Hydra.Core.KVDBRuntime as R
import qualified Hydra.Core.SqlDBRuntime as R
import qualified Database.RocksDB as Rocks
import qualified Database.Redis as Redis
import qualified Database.SQLite.Simple as SQLite

-- | Runtime data for the concrete logger impl.
newtype LoggerRuntime = LoggerRuntime
    { _hsLoggerHandle :: Maybe Impl.HsLoggerHandle
    }

-- | Runtime for processes.
data ProcessRuntime = ProcessRuntime
    { _idCounter :: IORef Int
    , _processes :: TVar (Map D.ProcessId ThreadId)   -- TODO: FIXME: should be TMVar
    }

-- | Runtime data for core subsystems.
data CoreRuntime = CoreRuntime
    { _rocksDBs       :: R.RocksDBHandles
    , _redisConns     :: R.RedisConnections
    -- , _sqliteConns    :: R.SQLiteDBConns
    , _loggerRuntime  :: LoggerRuntime
    , _stateRuntime   :: StateRuntime
    , _processRuntime :: ProcessRuntime
    , _sqlConns       :: MVar (Map D.ConnTag D.NativeSqlConn)
    }

-- | Logger that can be used in runtime via the logging subsystem.
newtype RuntimeLogger = RuntimeLogger
    { logMessage' :: D.LogLevel -> D.Message -> IO ()
    }

newtype VarHandle = VarHandle (TVar Any)

-- | State runtime.
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
  <$> newTMVarIO Map.empty
  <*> newTMVarIO Map.empty
  -- <*> newTMVarIO Map.empty
  <*> pure loggerRt
  <*> createStateRuntime
  <*> createProcessRuntime
  <*> newMVar Map.empty

clearProcessRuntime :: ProcessRuntime -> IO ()
clearProcessRuntime procRt = do
  -- TODO: should be TMVar for a better thread safety.
  processes <- atomically $ readTVar $ _processes procRt
  mapM_ killThread $ Map.elems processes

-- TODO: close DB handlers.
clearCoreRuntime :: CoreRuntime -> IO ()
clearCoreRuntime coreRt =
  (clearProcessRuntime $ _processRuntime coreRt)
  `finally` (R.closeRocksDBs $ _rocksDBs coreRt)
  `finally` (R.closeRedisConns $ _redisConns coreRt)
  -- TODO: close sql conns
  -- `finally` (R.closeSQLiteConns $ _sqliteConns coreRt)

-- TODO: Church version of flusher.
-- | Writes all stm entries into real logger.
flushStmLogger :: StateRuntime -> LoggerRuntime -> IO ()
flushStmLogger stateRt loggerRt = do
    l <- atomically $ do
            l <- readTVar $ _stmLog stateRt
            writeTVar (_stmLog stateRt) []
            pure l
    let loggerHandle = _hsLoggerHandle loggerRt
    mapM_ (\(D.LogEntry level msg) -> I.runLoggerL loggerHandle $ L.logMessage level msg) l
