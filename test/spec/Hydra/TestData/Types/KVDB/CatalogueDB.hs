{-# LANGUAGE DeriveAnyClass #-}

module Hydra.TestData.Types.KVDB.CatalogueDB where

import           Hydra.Prelude
import qualified Data.Aeson              as A
import qualified Data.ByteString.Lazy    as LBS
import           Text.Printf

import qualified Hydra.Domain            as D

import qualified Hydra.TestData.Types.Meteor     as D


data CatalogueDB

instance D.DB CatalogueDB where
  getDBName = "test_catalogue.rdb"


-- test_catalogue
-- ( meteors_count_key -> int
-- , meteor_idx|0 -> meteor_entity_json
-- )
-- ------------------------------------------------------------------
-- meteors_count 1
-- 0|0000000     {...}
-- 0|0000001     {...}

data MeteorEntity

instance D.DBEntity CatalogueDB MeteorEntity where
  data KeyEntity MeteorEntity = MeteorKey Int
    deriving (Show, Eq, Ord)
  data ValueEntity MeteorEntity = MeteorValue D.Meteor
    deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
  toDBKey (MeteorKey idx) = show $ toMeteorEntityKey idx
  toDBValue   = D.toDBValueJSON
  fromDBValue = D.fromDBValueJSON

instance D.AsKeyEntity MeteorEntity Int where
  toKeyEntity = MeteorKey

instance D.AsKeyEntity MeteorEntity D.Meteor where
  toKeyEntity = MeteorKey . D._id

instance D.AsValueEntity MeteorEntity D.Meteor where
  toValueEntity = MeteorValue
  fromValueEntity (MeteorValue v) = v


data MeteorsCountEntity

instance D.DBEntity CatalogueDB MeteorsCountEntity where
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
  fromValueEntity (MeteorsCountValue v) = v


meteorsCountKey :: D.KeyEntity MeteorsCountEntity
meteorsCountKey = D.toKeyEntity ("" :: String)

toIdxBase :: Int -> String
toIdxBase = printf "%07d"

toMeteorEntityKey :: Int -> String
toMeteorEntityKey = ("0|" <>) . toIdxBase
