{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Astro.KVDB.Entities.Meteor where

import           Hydra.Prelude

import qualified Data.Aeson            as A
import qualified Data.ByteString.Lazy  as LBS
import           Text.Printf

import qualified Hydra.Domain          as D

import qualified Astro.Domain.Meteor   as D
import           Astro.KVDB.Entities.DBs

-- catalogue
-- ( meteors_count_key -> int
-- , meteor_idx|0 -> meteor_entity_json
-- )
-- ------------------------------------------------------------------
-- meteors_count 1
-- 0|0000000     {...}
-- 0|0000001     {...}

data MeteorEntity

instance D.DBEntity CatalogueDB MeteorEntity where
  data KeyEntity CatalogueDB MeteorEntity = MeteorKey Int
    deriving (Show, Eq, Ord)
  data ValueEntity CatalogueDB MeteorEntity = MeteorValue D.Meteor
    deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

instance D.AsKeyEntity CatalogueDB MeteorEntity Int where
  toKeyEntity = MeteorKey

instance D.AsKeyEntity CatalogueDB MeteorEntity D.Meteor where
  toKeyEntity = MeteorKey . D._id

instance D.AsValueEntity CatalogueDB MeteorEntity D.Meteor where
  toValueEntity = MeteorValue
  fromValueEntity (MeteorValue v) = v

instance D.RawDBEntity CatalogueDB MeteorEntity where
  toDBKey (MeteorKey idx) = show $ toMeteorEntityKey idx
  toDBValue   = D.toDBValueJSON
  fromDBValue = D.fromDBValueJSON




data MeteorsCountEntity

instance D.DBEntity CatalogueDB MeteorsCountEntity where
  data KeyEntity CatalogueDB MeteorsCountEntity = MeteorsCountKey String
    deriving (Show, Eq, Ord)
  data ValueEntity CatalogueDB MeteorsCountEntity = MeteorsCountValue Int
    deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

instance D.AsKeyEntity CatalogueDB MeteorsCountEntity String where
  toKeyEntity _ = MeteorsCountKey "meteors_count"

instance D.AsValueEntity CatalogueDB MeteorsCountEntity Int where
  toValueEntity = MeteorsCountValue
  fromValueEntity (MeteorsCountValue v) = v

instance D.RawDBEntity CatalogueDB MeteorsCountEntity where
  toDBKey (MeteorsCountKey k) = show k
  toDBValue   = D.toDBValueJSON
  fromDBValue = D.fromDBValueJSON


meteorsCountKey :: D.KeyEntity CatalogueDB MeteorsCountEntity
meteorsCountKey = D.toKeyEntity ("" :: String)

toIdxBase :: Int -> String
toIdxBase = printf "%07d"

toMeteorEntityKey :: Int -> String
toMeteorEntityKey = ("0|" <>) . toIdxBase
