module Types where

import qualified Data.Map      as Map
import qualified Data.Set      as Set

import qualified Hydra.Domain  as D
import           Hydra.Prelude
import qualified Hydra.Runtime as R

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
  , maxMeteors    :: Maybe Int
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

doLogDiscovered = logDiscovered . _config
doLogTracked    = logTracked . _config
doLogTotal      = logTotal . _config
