{-# LANGUAGE DeriveAnyClass #-}

module Astro.SqlDB.CatalogueDB where

import Hydra.Prelude

import Database.Beam
import           Data.Time.Clock (UTCTime)


data DBMeteorT f = DBMeteor
  { _id          :: Columnar f Int
  , _size        :: Columnar f Int
  , _mass        :: Columnar f Int
  , _azimuth     :: Columnar f Int
  , _altitude    :: Columnar f Int
  , _timestamp   :: Columnar f UTCTime
  }
  deriving (Generic, Beamable)

type DBMeteor = DBMeteorT Identity
type DBMeteorId = PrimaryKey DBMeteorT Identity


instance Table DBMeteorT where
  data PrimaryKey DBMeteorT f = DBMeteorId (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = DBMeteorId . _id


data CatalogueDB f
  = CatalogueDB
  { _meteors :: f (TableEntity DBMeteorT)
  }
  deriving (Generic, Database be)

catalogueDB :: DatabaseSettings be CatalogueDB
catalogueDB = defaultDbSettings
