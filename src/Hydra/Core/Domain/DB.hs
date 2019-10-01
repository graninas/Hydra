{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE DeriveAnyClass         #-}

module Hydra.Core.Domain.DB where

import           Hydra.Prelude

import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as LBS

data DBErrorType
  = SystemError
  | KeyNotFound
  | InvalidType
  | DecodingFailed
  | UnknownDBError
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

data SqlDBType
  = SQLite
  deriving (Show, Read, Ord, Eq, Enum, Bounded, Generic, ToJSON, FromJSON)

type DBName = String

data DBHandle db = DBHandle DBType DBName
  deriving (Show, Generic)

data SqlDBHandle = SQLiteHandle SqlDBType DBName
