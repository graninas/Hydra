{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}


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

class DBEntity db entity | entity -> db where
  data KeyEntity entity :: *
  data ValueEntity entity :: *
  toDBKey     :: KeyEntity entity -> KVDBKey
  toDBValue   :: ValueEntity entity -> KVDBValue
  fromDBValue :: KVDBValue -> Maybe (ValueEntity entity)

class AsKeyEntity entity src where
  toKeyEntity :: src -> KeyEntity entity

class AsValueEntity entity src where
  toValueEntity   :: src -> ValueEntity entity
  fromValueEntity :: ValueEntity entity -> src

toDBKeyJSON :: ToJSON (KeyEntity entity) => KeyEntity entity -> KVDBKey
toDBKeyJSON = LBS.toStrict . A.encode

toDBValueJSON :: ToJSON (ValueEntity entity) => ValueEntity entity -> KVDBValue
toDBValueJSON = LBS.toStrict . A.encode

fromDBKeyJSON :: FromJSON (KeyEntity entity) => KVDBKey -> Maybe (KeyEntity entity)
fromDBKeyJSON = A.decode . LBS.fromStrict

fromDBValueJSON :: FromJSON (ValueEntity entity) => KVDBValue -> Maybe (ValueEntity entity)
fromDBValueJSON = A.decode . LBS.fromStrict
