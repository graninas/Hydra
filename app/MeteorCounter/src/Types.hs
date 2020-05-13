module Types where

import qualified Data.Map      as Map
import qualified Data.Set      as Set

import qualified Hydra.Domain  as D
import           Hydra.Prelude

data Region
  = NorthEast
  | NorthWest
  | SouthEast
  | SouthWest
  deriving (Show, Eq, Ord)

data Meteor = Meteor
  { _size   :: Int
  , _mass   :: Int
  , _region :: Region
  }
  deriving (Show, Eq, Ord)

type Meteors = D.StateVar (Set.Set Meteor)

type Catalogue = Map.Map Region Meteors

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
  { _catalogue    :: Catalogue
  , _totalMeteors :: D.StateVar Int
  , _channel      :: D.StateVar (Set.Set Meteor)
  , _config       :: AppConfig
  }

delaysEnabled :: AppState -> Bool
delaysEnabled = enableDelays . _config

storeTrackedMeteors :: AppState -> Bool
storeTrackedMeteors = storeTracked . _config

dFactor :: AppState -> Int
dFactor = delaysFactor . _config

doLogDiscovered :: AppState -> Bool
doLogDiscovered = logDiscovered . _config
doLogTracked :: AppState -> Bool
doLogTracked    = logTracked . _config
doLogTotal :: AppState -> Bool
doLogTotal      = logTotal . _config
