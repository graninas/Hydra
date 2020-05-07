{-# LANGUAGE RecordWildCards #-}
module Hydra.Core.SqlDBRuntime where

import           Hydra.Prelude

import qualified Hydra.Core.Domain               as D

import qualified Data.Pool                       as DP
import qualified Database.SQLite.Simple          as SQLite

bemToNative :: D.SqlConn beM -> D.NativeSqlConn
bemToNative (D.SQLiteConn _ conn) = D.NativeSQLiteConn conn
bemToNative (D.SQLitePool _ conn) = D.NativeSQLitePool conn

nativeToBem :: D.ConnTag -> D.NativeSqlConn -> D.SqlConn beM
nativeToBem connTag (D.NativeSQLiteConn conn) = D.SQLiteConn connTag conn
nativeToBem connTag (D.NativeSQLitePool conn) = D.SQLitePool connTag conn

connect' :: D.DBConfig beM -> IO (D.SqlConn beM)
connect' (D.SQLitePoolConf D.PoolConfig {..} dbName) = do
  pool <- DP.createPool (SQLite.open dbName) SQLite.close stripes keepAlive resourcesPerStripe
  sharedPool <- newMVar $ D.ResourceCreated pool
  pure $ D.SQLitePool dbName sharedPool
connect' (D.SQLiteConf dbName) = do
  conn <- SQLite.open dbName
  sharedConn <- newMVar $ D.ResourceCreated conn
  pure $ D.SQLiteConn dbName sharedConn
