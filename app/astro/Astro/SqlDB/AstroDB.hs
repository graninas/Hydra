{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

module Astro.SqlDB.AstroDB where

import Hydra.Prelude

import           Database.Beam
import           Data.Time.Clock (UTCTime)
import qualified Database.Beam as B
import           Database.Beam ((==.), (&&.), (<-.), (/=.), (==?.))

data MeteorT f = Meteor
    { _meteorId   :: B.C f Int
    , _meteorSize :: B.C f Int
    , _meteorMass :: B.C f Int
    , _meteorAzimuth    :: B.C f Int
    , _meteorAltitude   :: B.C f Int
    } deriving (Generic, B.Beamable)

instance B.Table MeteorT where
  data PrimaryKey MeteorT f =
    MeteorId (B.C f Int) deriving (Generic, B.Beamable)
  primaryKey = MeteorId . _meteorId

type Meteor = MeteorT Identity
type MeteorId = B.PrimaryKey MeteorT Identity

deriving instance Show Meteor
deriving instance Eq Meteor
deriving instance ToJSON Meteor
deriving instance FromJSON Meteor

data AstroDb f = AstroDb
    { _meteors :: f (B.TableEntity MeteorT)
    } deriving (Generic, B.Database be)

astroDb :: B.DatabaseSettings be AstroDb
astroDb = B.defaultDbSettings
