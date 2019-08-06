module Astro.Catalogue where

import qualified Data.Map       as Map
import qualified Data.Set       as Set

import qualified Hydra.Domain   as D
import qualified Hydra.Language as L
import           Hydra.Prelude
import qualified Hydra.Runtime  as R
import           Types

import qualified Astro.KVDB.Entries as E
import qualified Astro.Lens as Lens

withKBlocksDB
    ::
    -- forall s db a
    -- .  Lens.HasKBlocksDB s (D.Storage db)
    -- =>
    E.KVDBModel
    -> L.KVDBL db a
    -> L.LangL a
withKBlocksDB kvDBModel = L.withDatabase (kvDBModel ^. Lens.meteorsTable)

loadMeteorsCount :: E.KVDBModel -> L.LangL Int
loadMeteorsCount kvDBModel = do



  pure 10

dynamicsMonitor :: AppState -> L.LangL ()
dynamicsMonitor st = do
  meteorsCount <- loadMeteorsCount (st ^. kbdbModel)



  L.logInfo $ "Meteors count: " +|| meteorsCount


initState :: AppConfig -> L.AppL AppState
initState cfg = do
  eCatalogueDB :: D.DBResult CatalogueDB <- L.initKVDB
    $ D.KVDBConfig "catalogue"
    $ D.KVDBOptions True False

  catalogueDB <- case eCatalogue of
    Right db -> pure db
    Left err -> do
      L.logError $ "Failed to init KV DB catalogue: " +|| err
      error "Failed to init KV DB catalogue: " +|| err    -- TODO

  totalMeteors <- L.newStateVar 0

  pure $ AppState
    { _catalogueDB = catalogueDB
    , _totalMeteors = totalMeteors
    , _config = cfg
    }

astroCatalogue :: AppConfig -> L.AppL ()
astroCatalogue cfg = do
  appSt <- initState cfg

  L.process $ dynamicsMonitor appSt

  -- L.atomically $ do
  --   let maxTotal = fromMaybe 0 $ maxMeteors cfg
  --   total <- L.readVar $ _totalMeteors st
  --   when (maxTotal == 0 || total < maxTotal) L.retry

scenario :: R.CoreRuntime -> AppConfig -> IO ()
scenario coreRt cfg = void
  $ R.startApp coreRt
  $ meteorsMonitoring cfg
