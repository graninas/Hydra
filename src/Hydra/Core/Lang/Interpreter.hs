module Hydra.Core.Lang.Interpreter where

import           Hydra.Prelude

import qualified Data.Map as Map

import           Hydra.Core.ControlFlow.Interpreter         (runControlFlowL)
import qualified Hydra.Core.Language                        as L
import           Hydra.Core.Logger.Impl.HsLoggerInterpreter (runLoggerL)
import           Hydra.Core.Random.Interpreter              (runRandomL)
import qualified Hydra.Core.RLens                           as RLens
import qualified Hydra.Core.Runtime                         as R
import qualified Hydra.Core.KVDBRuntime                     as R
import qualified Hydra.Core.Domain                          as D
import           Hydra.Core.State.Interpreter               (runStateL)
import           Hydra.Core.KVDB.Interpreter                (runKVDBL)
import qualified Database.RocksDB                           as Rocks

evalRocksKVDB'
  :: R.CoreRuntime
  -> String
  -> L.KVDBL db a
  -> IO a
evalRocksKVDB' coreRt dbname act = do
  dbs <- atomically $ readTMVar $ coreRt ^. RLens.rocksDBs
  case Map.lookup dbname dbs of
    Nothing       -> error $ "Rocks KV DB not registered: " +| dbname |+ ""
    Just dbHandle -> runKVDBL dbHandle act

-- TODO: implement Redis
evalRedisKVDB'
  :: R.CoreRuntime
  -> String
  -> L.KVDBL db a
  -> IO a
evalRedisKVDB' coreRt dbname act = error "Not implemented yet."

evalKVDB'
  :: R.CoreRuntime
  -> D.DBHandle db
  -> L.KVDBL db a
  -> IO a
evalKVDB' coreRt (D.DBHandle dbtype dbname) act
  | dbtype == R.rocksdb = evalRocksKVDB' coreRt dbname act
  | dbtype == R.redisdb = evalRedisKVDB' coreRt dbname act
  | otherwise = error $ "Unknownd DB Type: " <> show dbtype

interpretLangF :: R.CoreRuntime -> L.LangF a -> IO a
interpretLangF coreRt (L.EvalStateAtomically action next) = do
    let stateRt  = coreRt ^. RLens.stateRuntime
    let loggerRt = coreRt ^. RLens.loggerRuntime
    res <- atomically $ runStateL stateRt action
    R.flushStmLogger stateRt loggerRt
    pure $ next res
interpretLangF coreRt (L.EvalLogger msg next) =
    next <$> runLoggerL (coreRt ^. RLens.loggerRuntime . RLens.hsLoggerHandle) msg
interpretLangF _      (L.EvalRandom  s next)        = next <$> runRandomL s
interpretLangF coreRt (L.EvalControlFlow f    next) = next <$> runControlFlowL coreRt f
interpretLangF _      (L.EvalIO f next)             = next <$> f
interpretLangF coreRt (L.EvalKVDB storage act next) = next <$> evalKVDB' coreRt storage act

runLangL :: R.CoreRuntime -> L.LangL a -> IO a
runLangL coreRt = foldFree (interpretLangF coreRt)
