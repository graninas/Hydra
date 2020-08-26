{-# LANGUAGE RecordWildCards #-}
module Hydra.Core.SqlDBRuntime where

import           Hydra.Prelude

import qualified Hydra.Core.Domain               as D

import qualified Data.Pool                       as DP
import qualified Database.SQLite.Simple          as SQLite

-- | These functions transform to and from runtime representation
-- of the connection.
bemToRuntime :: D.SqlConn beM -> D.RuntimeSqlConn
bemToRuntime (D.SQLitePool _ conn) = D.RuntimeSQLiteConn conn

runtimeToBem :: D.ConnTag -> D.RuntimeSqlConn -> D.SqlConn beM
runtimeToBem connTag (D.RuntimeSQLiteConn conn) = D.SQLitePool connTag conn

withTransaction :: D.SqlConn beM -> (D.NativeSqlConn -> IO a) -> IO (D.DBResult a)
withTransaction p actF =
  withNativeConnection p $ \nativeConn -> do
    eRes <- try $ bracketOnError (begin nativeConn) (const (rollback nativeConn)) $ const $ do
      res <- actF nativeConn
      commit nativeConn
      return res
    case eRes of
      Left (e :: SomeException) -> pure $ Left $ D.DBError D.TransactionRollbacked $ show e
      Right res -> pure $ Right res
  where
    withNativeConnection (D.SQLitePool _ pool) f = DP.withResource pool $ \conn -> f (D.NativeSQLiteConn conn)
    begin     (D.NativeSQLiteConn conn) = beginTransactionSQLite conn
    commit    (D.NativeSQLiteConn conn) = commitTransactionSQLite conn
    rollback  (D.NativeSQLiteConn conn) = rollbackTransactionSQLite conn

beginTransactionSQLite :: SQLite.Connection -> IO ()
beginTransactionSQLite conn = do
  SQLite.execute_ conn "PRAGMA busy_timeout = 60000"
  SQLite.execute_ conn "BEGIN TRANSACTION"

commitTransactionSQLite :: SQLite.Connection -> IO ()
commitTransactionSQLite conn = SQLite.execute_ conn "COMMIT TRANSACTION"

rollbackTransactionSQLite :: SQLite.Connection -> IO ()
rollbackTransactionSQLite conn = SQLite.execute_ conn "ROLLBACK TRANSACTION"

-- | Creating SqlConn using the config.
createSqlConn :: D.DBConfig beM -> IO (D.SqlConn beM)
createSqlConn (D.SQLitePoolConf connTag dbname D.PoolConfig {..}) = D.SQLitePool connTag
  <$> DP.createPool (SQLite.open dbname) SQLite.close stripes keepAlive resourcesPerStripe
