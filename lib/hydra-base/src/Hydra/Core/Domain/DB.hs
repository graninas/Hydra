{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE FunctionalDependencies #-}

module Hydra.Core.Domain.DB where

import           Hydra.Prelude

import           Data.Time.Clock                    (NominalDiffTime)


data DBErrorType
  = SystemError
  | KeyNotFound   -- TODO: should not be here.
  | InvalidType
  | DecodingFailed
  | UnknownDBError
  | SomeError
  | ConnectionIsDead
  | ConnectionAlreadyExists
  | ConnectionDoesNotExist
  | FailedToConnect
  | TransactionRollbacked
  deriving (Generic, Ord, Eq, Enum, Bounded, Show, Read)

data DBError = DBError DBErrorType Text
  deriving (Generic, Ord, Eq, Show, Read)

type DBResult a = Either DBError a

class DB db where
  getDBName :: DBName

data DBType
  = Redis
  | RocksDB
  | MockedKVDB
  deriving (Show, Read, Ord, Eq, Enum, Bounded, Generic, ToJSON, FromJSON)

type DBName = String

data DBHandle db = DBHandle DBType DBName
  deriving (Show, Generic)

data PoolConfig = PoolConfig
  { stripes :: Int
  , keepAlive :: NominalDiffTime
  , resourcesPerStripe :: Int
  } deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)


type ConnTag = String

class GetConnTag a where
  getConnTag :: a -> ConnTag
