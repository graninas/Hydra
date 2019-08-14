module Astro.Catalogue where

import qualified Data.Map       as Map
import qualified Data.Set       as Set

import qualified Hydra.Domain   as D
import qualified Hydra.Language as L
import           Hydra.Prelude
import qualified Hydra.Runtime  as R

import           Astro.Types
import           Astro.Lens
import           Astro.KVDB.Entities.Meteor
import           Astro.KVDB.Entities.DBs

withCatalogueDB :: AppState -> L.KVDBL CatalogueDB a -> L.LangL a
withCatalogueDB st = L.withKVDB (st ^. catalogueDB)

loadMeteorsCount :: AppState -> L.LangL Int
loadMeteorsCount st = do
  eCount <- withCatalogueDB st $ L.load' meteorsCountKey
  case eCount of
    Left err -> do
      L.logError ("Failed to get meteors count: " <> show err)
      pure 0
    Right n -> pure n

dynamicsMonitor :: AppState -> L.LangL ()
dynamicsMonitor st = do
  meteorsCount <- loadMeteorsCount st
  L.logInfo $ "Meteors count: " +|| meteorsCount ||+ ""
  L.delay $ 1000


initState :: AppConfig -> L.AppL AppState
initState cfg = do
  eCatalogueDB <- L.scenario
    $ L.initKVDB
    $ D.KVDBConfig @CatalogueDB "catalogue"
    $ D.KVDBOptions True False

  catalogueDB <- case eCatalogueDB of
    Right db -> pure db
    Left err -> do
      L.logError $ "Failed to init KV DB catalogue: " +|| err ||+ ""
      error $ "Failed to init KV DB catalogue: " +|| err ||+ ""    -- TODO

  totalMeteors <- L.newVarIO 0

  pure $ AppState
    { _catalogueDB = catalogueDB
    , _totalMeteors = totalMeteors
    , _config = cfg
    }

astroCatalogue :: AppConfig -> L.AppL ()
astroCatalogue cfg = do
  appSt <- initState cfg

  L.process $ dynamicsMonitor appSt

  -- L.awaitAppForever
  L.delay 10000000
