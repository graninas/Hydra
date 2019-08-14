{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DuplicateRecordFields  #-}

module Hydra.Core.Domain.KVDB where

import           Hydra.Prelude

import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as LBS

import           Hydra.Core.Domain.DB

data KVDBOptions = KVDBOptions
  { _createIfMissing :: Bool
  , _errorIfExists   :: Bool
  }
  deriving (Show, Generic)

data KVDBConfig db = KVDBConfig
  { _path    :: FilePath
  , _options :: KVDBOptions
  }
  deriving (Show, Generic)


type KVDBKey   = ByteString
type KVDBValue = ByteString

class DB db where
  getDBName :: FilePath

-- Domain data type /= DB data type.
-- DB data type can be very different.
-- Keys can be very different and can be obtained from different sources.
class DB db => DBEntity db entity where
  data KeyEntity   db entity :: *
  data ValueEntity db entity :: *

class DB db => AsKeyEntity db entity src where
  toKeyEntity :: src -> KeyEntity db entity

class DB db => AsValueEntity db entity src where
  toValueEntity   :: src -> ValueEntity db entity
  fromValueEntity :: ValueEntity db entity -> src

class DB db => RawDBEntity db entity where
  toDBKey     :: KeyEntity db entity   -> KVDBKey
  toDBValue   :: ValueEntity db entity -> KVDBValue
  fromDBValue :: KVDBValue -> Maybe (ValueEntity db entity)

toDBKeyJSON :: ToJSON (KeyEntity db entity) => KeyEntity db entity -> KVDBKey
toDBKeyJSON = LBS.toStrict . A.encode

toDBValueJSON :: ToJSON (ValueEntity db entity) => ValueEntity db entity -> KVDBValue
toDBValueJSON = LBS.toStrict . A.encode

fromDBKeyJSON :: FromJSON (KeyEntity db entity) => KVDBKey -> Maybe (KeyEntity db entity)
fromDBKeyJSON = A.decode . LBS.fromStrict

fromDBValueJSON :: FromJSON (ValueEntity db entity) => KVDBValue -> Maybe (ValueEntity db entity)
fromDBValueJSON = A.decode . LBS.fromStrict
