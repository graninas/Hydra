{-# LANGUAGE DeriveAnyClass #-}

module Labyrinth.KVDB.Model where

import           Labyrinth.Prelude
import           Labyrinth.Domain
import           Labyrinth.Types

import qualified Hydra.Domain as D

-- labyrinths
-- ( lab_key -> lab_entity_json
-- )


data GameEntity = GameEntity
  { geIdx             :: Int
  , geLab             :: Labyrinth
  , gePlayerPos       :: Pos
  , gePlayerHP        :: Int
  , gePlayerInventory :: Inventory
  , geBearPos         :: Pos
  }
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)


data LabKVDB

instance D.DB LabKVDB where
  getDBName = "lab.rdb"

instance D.DBEntity LabKVDB GameEntity where
  data KeyEntity GameEntity = LabKey Int
    deriving (Show, Eq, Ord)

  data ValueEntity GameEntity = LabVal GameEntity
    deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

  toDBKey (LabKey idx) = show idx
  toDBValue   = D.toDBValueJSON
  fromDBValue = D.fromDBValueJSON

instance D.AsKeyEntity GameEntity GameEntity where
  toKeyEntity = LabKey . geIdx

instance D.AsValueEntity GameEntity GameEntity where
  toValueEntity = LabVal
  fromValueEntity _ (LabVal ge) = ge
