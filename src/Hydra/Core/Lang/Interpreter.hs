module Hydra.Core.Lang.Interpreter where

import           Hydra.Prelude

import qualified Data.Map as Map

import           Hydra.Core.ControlFlow.Interpreter         (runControlFlowL)
import qualified Hydra.Core.Language                        as L
import           Hydra.Core.Logger.Impl.HsLoggerInterpreter (runLoggerL)
import           Hydra.Core.Random.Interpreter              (runRandomL)
import qualified Hydra.Core.RLens                           as RLens
import qualified Hydra.Core.Runtime                         as R
import qualified Hydra.Core.Domain                          as D
import           Hydra.Core.State.Interpreter               (runStateL)
import           Hydra.Core.KVDB.Interpreter                (runKVDBL)
import qualified Database.RocksDB                           as Rocks

import           System.FilePath                            ((</>))

initOptions :: D.KVDBOptions -> Rocks.Options
initOptions opts = Rocks.defaultOptions
  { Rocks.createIfMissing = D._createIfMissing opts
  , Rocks.errorIfExists   = D._errorIfExists opts
  }

initKVDB' :: R.CoreRuntime -> D.KVDBConfig db -> String -> IO (D.DBResult (D.KVDBStorage db))
initKVDB' coreRt (D.KVDBConfig path opts) dbName = do
  let dbPath = path </> dbName
  eDb <- try $ Rocks.open dbPath $ initOptions opts
  case eDb of
    Left (err :: SomeException) -> pure $ Left $ D.DBError D.SystemError $ show err
    Right db -> do
      dbM <- newMVar db
      atomically
        $ modifyTVar (coreRt ^. RLens.rocksDBs)
        $ Map.insert dbPath dbM
      pure $ Right $ D.KVDBStorage dbPath

evalKVDB'
  :: R.CoreRuntime
  -> D.KVDBStorage db
  -> L.KVDBL db a
  -> IO a
evalKVDB' coreRt (D.KVDBStorage path) act = do
  dbs <- readTVarIO $ coreRt ^. RLens.rocksDBs
  case Map.lookup path dbs of
    Nothing       -> error $ "KV DB not registered: " +| path |+ ""
    Just dbHandle -> runKVDBL dbHandle act

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
interpretLangF coreRt (L.InitKVDB cfg dbName next)  = next <$> initKVDB' coreRt cfg dbName
interpretLangF coreRt (L.EvalKVDB storage act next) = next <$> evalKVDB' coreRt storage act

runLangL :: R.CoreRuntime -> L.LangL a -> IO a
runLangL coreRt = foldFree (interpretLangF coreRt)
