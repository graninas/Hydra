{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE FunctionalDependencies #-}

module Hydra.Core.Domain.DB where

import           Hydra.Prelude

import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as LBS
import           Data.Time.Clock                    (NominalDiffTime)
import qualified Data.Pool                       as DP
import qualified Database.Beam                   as B
import qualified Database.Beam.Backend.SQL       as B
import qualified Database.Beam.Sqlite            as BS
import qualified Database.Beam.Sqlite.Connection as SQLite
import           Database.Beam.Sqlite            (Sqlite)
import qualified Database.SQLite.Simple          as SQLite


data DBErrorType
  = SystemError
  | KeyNotFound   -- TODO: should not be here.
  | InvalidType
  | DecodingFailed
  | UnknownDBError
  | SomeError
  | ConnectionIsDead
  | ConnectionAlreadyExists
  | FailedToConnect
  deriving (Generic, Ord, Eq, Enum, Bounded, Show, Read)

data DBError = DBError DBErrorType Text
  deriving (Generic, Ord, Eq, Show, Read)

type DBResult a = Either DBError a

class DB db where
  getDBName :: DBName

data DBType
  = Redis
  | RocksDB
  deriving (Show, Read, Ord, Eq, Enum, Bounded, Generic, ToJSON, FromJSON)

type DBName = String

data DBHandle db = DBHandle DBType DBName
  deriving (Show, Generic)

data SQLiteHandle = SQLiteHandle DBName

mkSQLiteHandle :: DBName -> SQLiteHandle
mkSQLiteHandle = SQLiteHandle

-----------------------
data PoolConfig = PoolConfig
  { stripes :: Int
  , keepAlive :: NominalDiffTime
  , resourcesPerStripe :: Int
  } deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)


type ConnTag = String

class GetConnTag a where
  getConnTag :: a -> ConnTag

data ResourceStatus conn
  = ResourceCreated conn
  | ResourceDisposed

type SharedResource conn = MVar (ResourceStatus conn)
