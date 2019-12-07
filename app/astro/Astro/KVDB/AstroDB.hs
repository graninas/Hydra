{-# LANGUAGE DeriveAnyClass #-}

module Astro.KVDB.AstroDB where

import           Hydra.Prelude

import qualified Data.Aeson              as A
import qualified Data.ByteString.Lazy    as LBS
import           Text.Printf

import qualified Hydra.Domain            as D
import qualified Astro.Domain.Meteor     as D

-- catalogue
-- ( meteors_count_key -> int
-- , meteor_idx|0 -> meteor_entity_json
-- )
-- ------------------------------------------------------------------
-- meteors_count 1
-- 0|0000000     {...}
-- 0|0000001     {...}

data AstroDB

instance D.DB AstroDB where
  getDBName = "astro.rdb"

-- ------------------------------------------------------------------

data MeteorEntity

instance D.DBEntity AstroDB MeteorEntity where
  data KeyEntity MeteorEntity = MeteorKey D.MeteorID
    deriving (Show, Eq, Ord)
  data ValueEntity MeteorEntity = KVDBMeteor
          { size  :: Int
          , mass  :: Int
          , azmt  :: Int
          , alt   :: Int
          , time  :: D.DateTime
          }
          deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
  toDBKey (MeteorKey idx) = show $ formatMeteorID idx
  toDBValue   = D.toDBValueJSON
  fromDBValue = D.fromDBValueJSON

instance D.AsKeyEntity MeteorEntity D.MeteorID where
  toKeyEntity = MeteorKey

instance D.AsKeyEntity MeteorEntity D.Meteor where
  toKeyEntity = MeteorKey . D.meteorId

instance D.AsValueEntity MeteorEntity D.Meteor where
  toValueEntity = toKVDBMeteor
  fromValueEntity (MeteorKey idx) = fromKVDBMeteor idx

mkMeteorKey :: D.MeteorID -> D.KeyEntity MeteorEntity
mkMeteorKey = D.toKeyEntity

formatMeteorID :: D.MeteorID -> String
formatMeteorID = ("0|" <>) . toIdxBase

toKVDBMeteor :: D.Meteor -> D.ValueEntity MeteorEntity
toKVDBMeteor (D.Meteor _ size mass (D.Coords azmt alt) time) = KVDBMeteor {..}

fromKVDBMeteor :: D.MeteorID -> D.ValueEntity MeteorEntity -> D.Meteor
fromKVDBMeteor meteorId KVDBMeteor {..} = D.Meteor
  { D.meteorId  = meteorId
  , D.size      = size
  , D.mass      = mass
  , D.coords    = D.Coords azmt alt
  , D.timestamp = time
  }

-- ------------------------------------------------------------------

data MeteorsCountEntity

instance D.DBEntity AstroDB MeteorsCountEntity where
  data KeyEntity MeteorsCountEntity = MeteorsCountKey String
    deriving (Show, Eq, Ord)
  data ValueEntity MeteorsCountEntity = MeteorsCountValue Int
    deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
  toDBKey (MeteorsCountKey k) = show k
  toDBValue   = D.toDBValueJSON
  fromDBValue = D.fromDBValueJSON

instance D.AsKeyEntity MeteorsCountEntity String where
  toKeyEntity _ = MeteorsCountKey "meteors_count"

instance D.AsValueEntity MeteorsCountEntity Int where
  toValueEntity = MeteorsCountValue
  fromValueEntity _ (MeteorsCountValue v) = v


meteorsCountKey :: D.KeyEntity MeteorsCountEntity
meteorsCountKey = D.toKeyEntity ("" :: String)

-- ------------------------------------------------------------------

toIdxBase :: Int -> String
toIdxBase = printf "%07d"
