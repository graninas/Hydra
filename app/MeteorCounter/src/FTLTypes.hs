module FTLTypes where

import qualified Data.Map      as Map
import qualified Data.Set      as Set

import           Hydra.Prelude
import           Hydra.FTL     as L
import           Types

type Meteors' m = L.StateVar m (Set.Set Meteor)

type Catalogue' m = Map.Map Region (Meteors' m)

data AppState' m = AppState'
  { _catalogue'    :: Catalogue' m
  , _totalMeteors' :: L.StateVar m Int
  , _channel'      :: L.StateVar m (Set.Set Meteor)
  , _config'       :: AppConfig
  }


delaysEnabled' :: AppState' m -> Bool
delaysEnabled' = enableDelays . _config'

dFactor' = delaysFactor . _config'

storeTrackedMeteors' :: AppState' m -> Bool
storeTrackedMeteors' = storeTracked . _config'

doLogDiscovered' = logDiscovered . _config'
doLogTracked'    = logTracked . _config'
doLogTotal'      = logTotal . _config'
