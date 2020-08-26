module Hydra.Core.Runtime where

import           Hydra.Prelude

import qualified Data.Map                        as Map

import qualified Hydra.Core.Domain               as D
import qualified Hydra.Core.Logger.Impl.HsLogger as Impl

import qualified Hydra.Core.KVDBRuntime as R

import           Network.HTTP.Client     (Manager, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified System.Mem             as SYSM (performGC)

-- | Runtime data for the concrete logger impl.
newtype LoggerRuntime = LoggerRuntime
    { _hsLoggerHandle :: Maybe Impl.HsLoggerHandle
    }

-- | Runtime for processes.
data ProcessRuntime = ProcessRuntime
    { _idCounter :: IORef Int
    , _processes :: TVar (Map D.ProcessId ThreadId)
    }

data CmdVerbosity
  = MessagesOnly
  | WithArgErrors
  | WithSkipErrors
  deriving (Show, Eq)

-- | Runtime data for core subsystems.
data CoreRuntime = CoreRuntime
    { _rocksDBs          :: R.RocksDBHandles
    , _redisConns        :: R.RedisConnections
    , _loggerRuntime     :: LoggerRuntime
    , _stateRuntime      :: StateRuntime
    , _processRuntime    :: ProcessRuntime
    , _sqlConns          :: MVar (Map D.ConnTag D.RuntimeSqlConn)
    , _httpClientManager :: Manager
    , _cmdVerbosity      :: CmdVerbosity
    }

-- | Logger that can be used in runtime via the logging subsystem.
newtype RuntimeLogger = RuntimeLogger
    { logMessage' :: D.LogLevel -> D.Message -> IO ()
    }

newtype VarHandle = VarHandle (TVar Any)

-- | State runtime.
data StateRuntime = StateRuntime
    { _varId  :: TVar D.VarId                       -- ^ Var id counter
    , _state  :: TMVar (Map.Map D.VarId VarHandle)  -- ^ Tracked variables
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

createCoreRuntime' :: LoggerRuntime -> CmdVerbosity -> IO CoreRuntime
createCoreRuntime' loggerRt verbosity = CoreRuntime
  <$> newTMVarIO Map.empty
  <*> newTMVarIO Map.empty
  <*> pure loggerRt
  <*> createStateRuntime
  <*> createProcessRuntime
  <*> newMVar Map.empty
  <*> newManager tlsManagerSettings
  <*> pure verbosity

createCoreRuntime :: LoggerRuntime -> IO CoreRuntime
createCoreRuntime loggerRt = createCoreRuntime' loggerRt WithArgErrors

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
  `finally` SYSM.performGC


withCoreRuntime :: Maybe D.LoggerConfig -> (CoreRuntime -> IO a) -> IO a
withCoreRuntime mbLoggerCfg coreF =
  bracket createLogger' clearLoggerRuntime $ \loggerRt ->
  bracket (createCoreRuntime loggerRt) clearCoreRuntime coreF
  where
    createLogger' = case mbLoggerCfg of
      Nothing        -> createVoidLoggerRuntime
      Just loggerCfg -> createLoggerRuntime loggerCfg
