{-# LANGUAGE DeriveAnyClass #-}

module Hydra.TestData.Types.SqlDB.CatalogueDB where

import Hydra.Prelude

import           Database.Beam
import           Data.Time.Clock (UTCTime)

data DBMeteorT f = DBMeteor
  { _id          :: Columnar f Int32
  , _size        :: Columnar f Int32
  , _mass        :: Columnar f Int32
  , _azimuth     :: Columnar f Int32
  , _altitude    :: Columnar f Int32
  , _timestamp   :: Columnar f UTCTime
  }
  deriving (Generic, Beamable)

type DBMeteor = DBMeteorT Identity
type DBMeteorId = PrimaryKey DBMeteorT Identity

-- CREATE TABLE meteors (id INT NOT NULL AUTOINCREMENT, size INT NOT NULL, mass INT NOT NULL, azimuth INT NOT NULL, altitude INT NOT NULL, timestamp TIMESTAMP NULL, PRIMARY KEY( id ));
-- CREATE TABLE meteors (id INTEGER PRIMARY KEY AUTOINCREMENT, size INT NOT NULL, mass INT NOT NULL, azimuth INT NOT NULL, altitude INT NOT NULL, timestamp TIMESTAMP NULL);

instance Table DBMeteorT where
  data PrimaryKey DBMeteorT f = DBMeteorId (Columnar f Int32)
    deriving (Generic, Beamable)
  primaryKey = DBMeteorId . _id


data CatalogueDB f
  = CatalogueDB
  { _meteors :: f (TableEntity DBMeteorT)
  }
  deriving (Generic, Database be)

catalogueDB :: DatabaseSettings be CatalogueDB
catalogueDB = defaultDbSettings
