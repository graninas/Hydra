{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Astro.KVDB.Entities.Meteor where

import           Hydra.Prelude

import qualified Data.Aeson            as A
import qualified Data.ByteString.Lazy  as LBS
import           Text.Printf

import qualified Hydra.Domain          as D

import qualified Astro.Domain.Meteor   as D

-- catalogue
-- ( meteors_count_key -> int
-- , meteor_idx|0 -> meteor_entity_json
-- )
-- ------------------------------------------------------------------
-- meteors_count 1
-- 0|0000000     {...}
-- 0|0000001     {...}

data MeteorEntity
data MeteorsCountEntity

-- MeteorEntity

instance D.DBEntity MeteorEntity where
  data KeyEntity MeteorEntity = MeteorKey Int
    deriving (Show, Eq, Ord)
  data ValueEntity MeteorEntity = MeteorValue D.Meteor
    deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

instance D.AsKeyEntity MeteorEntity Int where
  toKeyEntity = MeteorKey

instance D.AsKeyEntity MeteorEntity D.Meteor where
  toKeyEntity = MeteorKey . D._id

instance D.AsValueEntity MeteorEntity D.Meteor where
  toValueEntity = MeteorValue
  fromValueEntity (MeteorValue v) = v

instance D.RawDBEntity MeteorEntity where
  toDBKey (MeteorKey idx) = show $ toMeteorEntityKey idx
  toDBValue   = D.toDBValueJSON
  fromDBValue = D.fromDBValueJSON

-- instance D.DBModelEntity CatalogueDB MeteorEntity



-- instance D.DBModelEntity CatalogueDB MeteorsCountEntity


-- instance D.RawDBEntity CatalogueDB MeteorEntity where
--   toRawDBKey (MeteorKey idx) = A.encode kBlockIdx
--   toRawDBValue = LBS.toStrict . A.encode
--   fromRawDBValue = A.decode . LBS.fromStrict

-- -- KBlock entity
--
-- instance D.DBEntity KBlockEntity where
--     data DBKey   KBlockEntity = KBlockKey D.BlockNumber
--         deriving (Show, Eq, Ord)
--     data DBValue KBlockEntity = KBlockValue D.BlockTime D.BlockNumber D.Nonce D.Solver
--         deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
--
-- instance D.ToDBKey KBlockEntity D.BlockNumber where
--     toDBKey = KBlockKey
--
-- instance D.ToDBKey KBlockEntity D.KBlock where
--     toDBKey = KBlockKey . D._number
--
-- instance D.ToDBValue KBlockEntity D.KBlock where
--     toDBValue (D.KBlock time _ number nonce solver) = KBlockValue time number nonce solver
--
-- instance D.RawDBEntity KBlocksDB KBlockEntity where
--     toRawDBKey (KBlockKey kBlockIdx) = encodeUtf8 $ toKBlockEntityKeyBase kBlockIdx
--     toRawDBValue = LBS.toStrict . A.encode
--     fromRawDBValue = A.decode . LBS.fromStrict


toIdxBase :: Int -> String
toIdxBase = printf "%07d"

toMeteorEntityKey :: Int -> String
toMeteorEntityKey = ("0|" <>) . toIdxBase
