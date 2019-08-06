{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DuplicateRecordFields  #-}

module Hydra.Core.Domain.KVDB where

import           Hydra.Prelude

import           Data.Aeson.Extra     (noLensPrefix)
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
    , _options :: DBOptions
    }
    deriving (Show, Generic)


type DBKey   = ByteString
type DBValue = ByteString

-- class DB db where
--     getDbName :: FilePath
--
-- class DBEntity entity where
--     data DBKey   entity :: *
--     data DBValue entity :: *
--
-- class DBEntity entity => ToDBKey entity src where
--     toDBKey :: src -> DBKey entity
--
-- class DBEntity entity => ToDBValue entity src where
--     toDBValue :: src -> DBValue entity
--
-- class (DB db, DBEntity entity) => DBModelEntity db entity
--
-- class DBModelEntity db entity => RawDBEntity db entity where
--     toRawDBKey     :: DBKey   entity -> DBKey
--     toRawDBValue   :: DBValue entity -> DBValue
--     fromRawDBValue :: DBValue -> Maybe (DBValue entity)
--
--     -- TODO: this doesn't work by some strange reason.
--     default toRawDBValue :: ToJSON (DBValue entity) => DBValue entity -> DBValue
--     toRawDBValue = LBS.toStrict . A.encode
--     default fromRawDBValue :: FromJSON (DBValue entity) => DBValue -> Maybe (DBValue entity)
--     fromRawDBValue = A.decode . LBS.fromStrict
--
-- type DBE entity = (DBKey entity, DBValue entity)
--
-- defaultDbOptions :: DBOptions
-- defaultDbOptions = DBOptions
--     { _createIfMissing = False
--     , _errorIfExists   = False
--     }
--
-- toDBEntity
--     :: (ToDBKey entity src, ToDBValue entity src)
--     => src
--     -> DBE entity
-- toDBEntity src = (toDBKey src, toDBValue src)
--
-- instance ToJSON   (Storage db) where toJSON    = genericToJSON    noLensPrefix
-- instance FromJSON (Storage db) where parseJSON = genericParseJSON noLensPrefix
--
-- instance ToJSON   DBOptions where toJSON    = genericToJSON    noLensPrefix
-- instance FromJSON DBOptions where parseJSON = genericParseJSON noLensPrefix
--
-- instance ToJSON   (DBConfig db) where toJSON    = genericToJSON    noLensPrefix
-- instance FromJSON (DBConfig db) where parseJSON = genericParseJSON noLensPrefix
