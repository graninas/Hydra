{-# LANGUAGE RecordWildCards #-}
module Hydra.Core.SqlDBRuntime where

import           Hydra.Prelude

import qualified Data.Map                        as Map

import qualified Hydra.Core.Domain               as D
import qualified Hydra.Core.Language             as L

import qualified Data.Pool                       as DP
import qualified Database.Beam                   as B
import qualified Database.Beam.Backend.SQL       as B
import qualified Database.Beam.Sqlite            as BS
import qualified Database.Beam.Sqlite.Connection as SQLite
import           Database.Beam.Sqlite            (Sqlite)
import qualified Database.SQLite.Simple          as SQLite


type SQLiteDBConn   = MVar SQLite.Connection
type SQLiteDBConns  = TMVar (Map D.DBName SQLiteDBConn)

initSQLiteDB'
  :: SQLiteDBConns
  -> D.SQLiteConfig
  -> IO (D.DBResult D.SQLiteHandle)
initSQLiteDB' connsVar cfg@(D.SQLiteConfig dbName) = do
  eConn <- try $ SQLite.open dbName
  case eConn of
    Left (err :: SomeException) -> pure $ Left $ D.DBError D.SystemError $ show err
    Right conn -> do
      dbM <- newMVar conn
      atomically $ do
        conns <- takeTMVar connsVar
        putTMVar connsVar $ Map.insert dbName dbM conns
      pure $ Right $ D.mkSQLiteHandle dbName

deInitSQLiteConn :: SQLiteDBConn -> IO ()
deInitSQLiteConn connVar = do
  conn <- takeMVar connVar
  SQLite.close conn
  putMVar connVar conn

closeSQLiteConns :: SQLiteDBConns -> IO ()
closeSQLiteConns handleMapVar = do
  handleMap <- atomically $ takeTMVar handleMapVar
  mapM_ deInitSQLiteConn $ Map.elems handleMap
  atomically $ putTMVar handleMapVar Map.empty

-----------------
bemToNative :: D.SqlConn beM -> D.NativeSqlConn
bemToNative (D.SQLiteConn _ conn) = D.NativeSQLiteConn conn
bemToNative (D.SQLitePool _ conn) = D.NativeSQLitePool conn


connect' :: D.DBConfig beM -> IO (D.SqlConn beM)
connect' (D.SQLitePoolConf D.PoolConfig {..} dbName) = do
  pool <- DP.createPool (SQLite.open dbName) SQLite.close stripes keepAlive resourcesPerStripe
  sharedPool <- newMVar $ D.ResourceCreated pool
  pure $ D.SQLitePool dbName sharedPool
connect' (D.SQLiteConf dbName) = do
  conn <- SQLite.open dbName
  sharedConn <- newMVar $ D.ResourceCreated conn
  pure $ D.SQLiteConn dbName sharedConn
