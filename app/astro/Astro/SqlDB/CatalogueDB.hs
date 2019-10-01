{-# LANGUAGE DeriveAnyClass #-}

module Astro.SqlDB.CatalogueDB where

import Hydra.Prelude

import Database.Beam


data MeteorT f = Meteor
  { _id          :: Columnar f Int
  , _size        :: Columnar f Int
  , _mass        :: Columnar f Int
  , _azimuth     :: Columnar f Int
  , _altitude    :: Columnar f Int
  , _timestamp   :: Columnar f UTCTime
  }
  deriving (Show, Eq, Ord, Generic, Beamable)

type Meteor = MeteorT Identity
type MeteorId = PrimaryKey MeteorT Identity


instance Table MeteorT where
  data PrimaryKey MeteorT f = MeteorId (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = MeteorId . _id


data CatalogueDB f
  = CatalogueDB
  { _meteors :: f (TableEntity MeteorT)
  }
  deriving (Generic, Database be)

catalogueDB :: DatabaseSettings be CatalogueDB
catalogueDB = defaultDbSettings
