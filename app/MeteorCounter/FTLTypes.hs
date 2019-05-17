module FTLTypes where

import qualified Data.Map      as Map
import qualified Data.Set      as Set

import qualified Hydra.Domain  as D
import           Hydra.Prelude
import qualified Hydra.Runtime as R
import           Types

type Meteors' = TVar (Set.Set Meteor)

type Catalogue' = Map.Map Region Meteors'

data AppState' = AppState'
  { _catalogue'    :: Catalogue'
  , _totalMeteors' :: TVar Int
  , _channel'      :: TVar (Set.Set Meteor)
  , _config'       :: AppConfig
  }

delaysEnabled' :: AppState' -> Bool
delaysEnabled' = enableDelays . _config'

doLogDiscovered' = logDiscovered . _config'
doLogTracked'    = logTracked . _config'
doLogTotal'      = logTotal . _config'
