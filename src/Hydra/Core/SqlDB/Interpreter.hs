module Hydra.Core.SqlDB.Interpreter where

import           Hydra.Prelude
import qualified Data.Map as Map

import qualified Hydra.Core.Language as L
import qualified Hydra.Core.RLens    as RLens
import qualified Hydra.Core.Runtime  as R
import qualified Hydra.Core.Domain   as D
import           Hydra.Core.Logger.Impl.HsLoggerInterpreter (runLoggerL)

import qualified Database.Beam.Sqlite as SQLite
import           Database.Beam.Query (runSelectReturningList)
import           Database.Beam.Sqlite.Connection (runBeamSqliteDebug)
import           Database.Beam.Sqlite (Sqlite)

-- TODO: transactions
-- TODO: remove dep on HsLoggerInterpreter
interpretSQLiteDBF :: R.CoreRuntime -> D.DBName -> L.SqlDBF Sqlite a -> IO a
interpretSQLiteDBF coreRt dbName (L.RunBeamSelect selectQ next) = do
  let loggerHandle = coreRt ^. RLens.loggerRuntime . RLens.hsLoggerHandle
  let loggerF msg = runLoggerL loggerHandle $ L.logDebug $ toText msg
  let connsVar = coreRt ^. RLens.sqliteConns
  conns <- atomically $ readTMVar connsVar
  case Map.lookup dbName conns of
    Nothing -> pure $ next $ Left $ D.DBError D.ConnectionIsDead $ toText dbName
    Just connVar -> do
      conn <- readMVar connVar
      rs <- SQLite.runBeamSqliteDebug loggerF conn
              $ runSelectReturningList
              $ selectQ
      pure $ next $ Right rs


runSQLiteDBL :: R.CoreRuntime -> D.SQLiteHandle -> L.SqlDBL Sqlite a -> IO a
runSQLiteDBL coreRt (D.SQLiteHandle dbName) act = foldFree (interpretSQLiteDBF coreRt dbName) act
