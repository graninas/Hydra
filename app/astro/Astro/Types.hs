module Astro.Types where

import qualified Data.Map      as Map
import qualified Data.Set      as Set
import           Data.Time.Clock (UTCTime)

import qualified Hydra.Domain  as D
import           Hydra.Prelude
import qualified Hydra.Runtime as R

import           Astro.KVDB.Entities.DBs

type DateTime = UTCTime

data Coords = Coords
  { _azimuth  :: Int
  , _altitude :: Int
  }
  deriving (Show, Eq, Ord)

data Meteor = Meteor
  { _size        :: Int
  , _mass        :: Int
  , _coords      :: Coords
  , _timestamp   :: DateTime
  }
  deriving (Show, Eq, Ord)

type Meteors = D.StateVar (Set.Set Meteor)

data AppConfig = AppConfig
  { enableDelays  :: Bool
  , delaysFactor  :: Int
  }
  deriving (Show, Read, Eq, Ord)

data AppState = AppState
  { _catalogueDB  :: D.DBHandle CatalogueDB
  , _totalMeteors :: D.StateVar Int
  , _config       :: AppConfig
  }

-- delaysEnabled :: AppState -> Bool
-- delaysEnabled = enableDelays . _config
--
-- storeTrackedMeteors :: AppState -> Bool
-- storeTrackedMeteors = storeTracked . _config
--
-- dFactor = delaysFactor . _config
--
-- doLogDiscovered = logDiscovered . _config
-- doLogTracked    = logTracked . _config
-- doLogTotal      = logTotal . _config
