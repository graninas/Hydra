module Types where

import qualified Data.Map      as Map
import qualified Data.Set      as Set
import           Data.Time.Clock (UTCTime)

import qualified Hydra.Domain  as D
import           Hydra.Prelude
import qualified Hydra.Runtime as R

import           Astro.KVDB.Model

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

-- type Catalogue = Map.Map Region Meteors

data AppConfig = AppConfig
  { enableDelays  :: Bool
  , delaysFactor  :: Int
  , maxMeteors    :: Maybe Int
  , storeTracked  :: Bool
  , logDiscovered :: Bool
  , logTracked    :: Bool
  , logTotal      :: Bool
  }
  deriving (Show, Read, Eq, Ord)

data AppState = AppState
  { _catalogueDB  :: D.KVDBConn CatalogueDB
  , _totalMeteors :: D.StateVar Int
  , _config       :: AppConfig
  }
--
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
