{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE DeriveAnyClass         #-}


module Hydra.Core.Domain.SQLDB where

import           Hydra.Prelude

import qualified Data.Pool                       as DP
import qualified Database.Beam                   as B
import qualified Database.Beam.Backend.SQL       as B
import qualified Database.Beam.Sqlite            as BS
import qualified Database.Beam.Sqlite.Connection as SQLite
import qualified Database.SQLite.Simple          as SQLite

import           Hydra.Core.Domain.DB

class (B.BeamSqlBackend be, B.MonadBeam be beM) => BeamRuntime be beM
  | be -> beM, beM -> be where
  rtSelectReturningList :: B.FromBackendRow be a => B.SqlSelect be a -> beM [a]
  rtSelectReturningOne :: B.FromBackendRow be a => B.SqlSelect be a -> beM (Maybe a)
  rtInsert :: B.SqlInsert be table -> beM ()
  rtUpdate :: B.SqlUpdate be table -> beM ()
  rtDelete :: B.SqlDelete be table -> beM ()

class BeamRunner beM where
  getBeamDebugRunner :: SqlConn beM -> beM a -> ((String -> IO ()) -> IO a)

instance BeamRuntime BS.Sqlite BS.SqliteM where
  rtSelectReturningList = B.runSelectReturningList
  rtSelectReturningOne = B.runSelectReturningOne
  rtInsert = B.runInsert
  rtUpdate = B.runUpdate
  rtDelete = B.runDelete

instance BeamRunner BS.SqliteM where
  getBeamDebugRunner (SQLitePool tag pool) beM
    = \logger -> DP.withResource pool
    $ \conn -> SQLite.runBeamSqliteDebug logger conn beM

data NativeSqlConn
  = NativeSQLitePool (DP.Pool SQLite.Connection)

data SqlConn beM
  = SQLitePool ConnTag (DP.Pool SQLite.Connection)
  deriving (Generic)

data DBConfig beM
  = SQLitePoolConf DBName ConnTag PoolConfig
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- TODO: default idle time - ?
mkSQLiteConfig :: DBName -> DBConfig BS.SqliteM
mkSQLiteConfig dbname = SQLitePoolConf dbname dbname $ PoolConfig 1 1000000 1

mkSQLitePoolConfig :: DBName -> PoolConfig -> DBConfig BS.SqliteM
mkSQLitePoolConfig dbname = SQLitePoolConf dbname dbname

instance GetConnTag (SqlConn beM) where
  getConnTag (SQLitePool tag _) = tag

instance GetConnTag (DBConfig beM) where
  getConnTag (SQLitePoolConf _ connTag _) = connTag
