{-# LANGUAGE BangPatterns #-}

module Hydra.Core.Lang.Interpreter where

import           Hydra.Prelude

import qualified Data.Map as Map
import           Control.Exception (throwIO)

import           Hydra.Core.ControlFlow.Interpreter         (runControlFlowL)
import qualified Hydra.Core.Language                        as L
import           Hydra.Core.Logger.Impl.HsLoggerInterpreter (runLoggerL)
import           Hydra.Core.Random.Interpreter              (runRandomL)
import qualified Hydra.Core.RLens                           as RLens
import qualified Hydra.Core.Runtime                         as R
import qualified Hydra.Core.Domain                          as D
import           Hydra.Core.State.Interpreter               (runStateL)
import           Hydra.Core.KVDB.Interpreter                (runAsRocksDBL, runAsRedisL)
import           Hydra.Core.SqlDB.Interpreter               (runSqlDBL)
import qualified Hydra.Core.SqlDBRuntime                    as R
import qualified Servant.Client                             as S

evalRocksKVDB'
  :: R.CoreRuntime
  -> String
  -> L.KVDBL db a
  -> IO a
evalRocksKVDB' coreRt dbname act = do
  dbs <- atomically $ readTMVar $ coreRt ^. RLens.rocksDBs
  case Map.lookup dbname dbs of
    Nothing       -> error $ "Rocks KV DB not registered: " +| dbname |+ ""
    Just dbHandle -> runAsRocksDBL dbHandle act

evalRedisKVDB'
  :: R.CoreRuntime
  -> String
  -> L.KVDBL db a
  -> IO a
evalRedisKVDB' coreRt dbname act = do
  conns <- atomically $ readTMVar $ coreRt ^. RLens.redisConns
  case Map.lookup dbname conns of
    Nothing   -> error $ "Redis KV DB not registered: " +| dbname |+ ""
    Just conn -> runAsRedisL conn act

evalKVDB'
  :: R.CoreRuntime
  -> D.DBHandle db
  -> L.KVDBL db a
  -> IO a
evalKVDB' coreRt (D.DBHandle D.RocksDB dbname) act =
  evalRocksKVDB' coreRt dbname act
evalKVDB' coreRt (D.DBHandle D.Redis   dbname) act =
  evalRedisKVDB' coreRt dbname act

interpretLangF :: R.CoreRuntime -> L.LangF a -> IO a
interpretLangF coreRt (L.EvalStateAtomically action next) = do
    let stateRt  = coreRt ^. RLens.stateRuntime
    let loggerRt = coreRt ^. RLens.loggerRuntime
    res <- atomically $ runStateL stateRt action
    R.flushStmLogger stateRt loggerRt
    pure $ next res
interpretLangF coreRt (L.EvalLogger loggerAct next) =
    next <$> runLoggerL (coreRt ^. RLens.loggerRuntime . RLens.hsLoggerHandle) loggerAct
interpretLangF _      (L.EvalRandom  s next)        = next <$> runRandomL s
interpretLangF coreRt (L.EvalControlFlow f    next) = next <$> runControlFlowL coreRt f
interpretLangF _      (L.EvalIO f next)             = do
  !r <- f
  pure $ next r
interpretLangF coreRt (L.EvalKVDB storage act next) = next <$> evalKVDB' coreRt storage act
interpretLangF coreRt (L.EvalSqlDB conn sqlDbMethod next) = do
  let dbgLogger = runLoggerL (coreRt ^. RLens.loggerRuntime . RLens.hsLoggerHandle)
                . L.logMessage D.Debug . show
  -- TODO: transactions
  eRes <- try $ runSqlDBL conn dbgLogger sqlDbMethod
  pure $ next $ case eRes of
    Left (err :: SomeException) -> Left $ D.DBError D.SystemError $ show err
    Right res -> Right res

interpretLangF coreRt (L.GetSqlDBConnection cfg next) = do
  let connTag = D.getConnTag cfg
  let connsVar = coreRt ^. RLens.sqlConns
  connMap <- readMVar connsVar
  case Map.lookup connTag connMap of
    Just conn -> pure $ next $ Right $ R.nativeToBem connTag conn
    Nothing -> pure
      $ next $ Left $ D.DBError D.ConnectionDoesNotExist
      $ "Connection for " <> show connTag <> " not found."

interpretLangF _      (L.ThrowException exc _) = throwIO exc

interpretLangF coreRt (L.RunSafely act next) = do
  eResult <- try $ runLangL coreRt act
  pure $ next $ case eResult of
    Left (err :: SomeException) -> Left $ show err
    Right r  -> Right r

interpretLangF coreRt (L.CallServantAPI bUrl clientAct next)
  = next <$> catchAny
      (S.runClientM clientAct (S.mkClientEnv (coreRt ^. RLens.httpClientManager) bUrl))
      (pure . Left . S.ConnectionError)

runLangL :: R.CoreRuntime -> L.LangL a -> IO a
runLangL coreRt = foldFree (interpretLangF coreRt)
