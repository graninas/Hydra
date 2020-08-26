{-# LANGUAGE RecordWildCards #-}
module Hydra.Core.SqlDBRuntime where

import           Hydra.Prelude

import qualified Hydra.Core.Domain               as D

import qualified Data.Pool                       as DP
import qualified Database.SQLite.Simple          as SQLite

bemToNative :: D.SqlConn beM -> D.NativeSqlConn
bemToNative (D.SQLitePool _ conn) = D.NativeSQLitePool conn

nativeToBem :: D.ConnTag -> D.NativeSqlConn -> D.SqlConn beM
nativeToBem connTag (D.NativeSQLitePool conn) = D.SQLitePool connTag conn



createSqlConn :: D.DBConfig beM -> IO (D.SqlConn beM)
createSqlConn (D.SQLitePoolConf connTag dbname D.PoolConfig {..}) = D.SQLitePool connTag
  <$> DP.createPool (SQLite.open dbname) SQLite.close stripes keepAlive resourcesPerStripe
