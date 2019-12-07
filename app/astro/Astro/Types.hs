module Astro.Types where

import qualified Data.Map      as Map
import qualified Data.Set      as Set
import           Data.Time.Clock (UTCTime)

import qualified Hydra.Domain  as D
import           Hydra.Prelude
import qualified Hydra.Runtime as R

import           Astro.KVDB.Entities.DBs


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
