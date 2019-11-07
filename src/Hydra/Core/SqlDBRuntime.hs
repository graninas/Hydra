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
