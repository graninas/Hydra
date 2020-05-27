{-# LANGUAGE DeriveAnyClass #-}

module Labyrinth.KVDB.Model where

import           Labyrinth.Prelude
import           Labyrinth.Domain
import           Labyrinth.Types

import qualified Hydra.Domain as D

-- labyrinths
-- ( lab_key -> lab_entity_json
-- )

data LabKVDB

instance D.DB LabKVDB where
  getDBName = "lab.rdb"

data LabEntity

instance D.DBEntity LabKVDB LabEntity where
  data KeyEntity LabEntity = LabKey Int
    deriving (Show, Eq, Ord)

  data ValueEntity LabEntity = LabVal GameInfo
    deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
  toDBKey (LabKey idx) = show idx
  toDBValue   = D.toDBValueJSON
  fromDBValue = D.fromDBValueJSON

instance D.AsKeyEntity LabEntity Int where
  toKeyEntity = LabKey

instance D.AsValueEntity LabEntity GameInfo where
  toValueEntity = LabVal
  fromValueEntity _ (LabVal gi) = gi
