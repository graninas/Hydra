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

withMVar :: MVar a -> (a -> IO b) -> IO b
withMVar mvar = bracket (takeMVar mvar) (putMVar mvar)

withSharedResource :: Text -> SharedResource r -> (r -> IO b) -> IO b
withSharedResource tag mvar act = withMVar mvar act'
  where
    act' (ResourceCreated r) = act r
    act' ResourceDisposed    = error $ tag <> " was disposed." -- TODO: proper error

instance BeamRunner BS.SqliteM where
  getBeamDebugRunner (SQLiteConn tag sharedConn) beM = \logger ->
    withSharedResource ("Conn " +| tag |+ "") sharedConn
    $ \conn -> SQLite.runBeamSqliteDebug logger conn beM
  getBeamDebugRunner (SQLitePool tag sharedPool) beM = \logger ->
    withSharedResource ("Pool " +| tag |+ "") sharedPool
    $ \pool -> DP.withResource pool
    $ \conn -> SQLite.runBeamSqliteDebug logger conn beM

data NativeSqlConn
  = NativeSQLiteConn (SharedResource SQLite.Connection)
  | NativeSQLitePool (SharedResource (DP.Pool SQLite.Connection))

data SqlConn beM
  = SQLiteConn ConnTag (SharedResource SQLite.Connection)
  | SQLitePool ConnTag (SharedResource (DP.Pool SQLite.Connection))
  deriving (Generic)

data DBConfig beM
  = SQLiteConf DBName
  | SQLitePoolConf PoolConfig DBName
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkSQLiteConfig :: DBName -> DBConfig BS.SqliteM
mkSQLiteConfig = SQLiteConf

mkSQLitePoolConfig :: PoolConfig -> DBName -> DBConfig BS.SqliteM
mkSQLitePoolConfig = SQLitePoolConf

instance GetConnTag (SqlConn beM) where
  getConnTag (SQLiteConn tag _) = tag
  getConnTag (SQLitePool tag _) = tag

instance GetConnTag (DBConfig beM) where
  getConnTag (SQLiteConf tag) = tag
  getConnTag (SQLitePoolConf _ tag) = tag
