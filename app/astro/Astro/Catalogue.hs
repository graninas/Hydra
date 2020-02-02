module Astro.Catalogue where

import qualified Data.Map       as Map
import qualified Data.Set       as Set
import qualified Data.Time.Clock as Time

import qualified Hydra.Domain   as D
import qualified Hydra.Language as L
import           Hydra.Prelude
import qualified Hydra.Runtime  as R

import qualified Database.Beam as B
import qualified Database.Beam.Sqlite as BS
import qualified Database.Beam.Backend.SQL as B
import           Database.Beam ((==.), (&&.), (<-.), (/=.), (==?.))

import           Astro.Types
import           Astro.Lens
import           Astro.Domain.Meteor
import qualified Astro.KVDB.AstroDB as KVDB
import qualified Astro.SqlDB.AstroDB as SqlDB

withAstroKVDB :: AppState -> L.KVDBL KVDB.AstroDB a -> L.LangL a
withAstroKVDB st = L.withKVDB (st ^. astroKVDB)

loadMeteor
  :: D.DBHandle KVDB.AstroDB
  -> L.LangL (D.DBResult Meteor)
loadMeteor astroDB = L.withKVDB astroDB
  $ L.loadEntity
  $ KVDB.mkMeteorKey 0

loadMeteorsCount :: AppState -> L.LangL Int
loadMeteorsCount st = do
  eCount <- withAstroKVDB st $ L.loadEntity KVDB.meteorsCountKey
  case eCount of
    Left err -> do
      L.logError ("Failed to get meteors count: " <> show err)
      pure 0
    Right n -> pure n

dynamicsMonitor :: AppState -> L.LangL ()
dynamicsMonitor st = do
  meteorsCount <- loadMeteorsCount st
  L.logInfo $ "Meteors count: " +|| meteorsCount ||+ ""
  L.delay 1000


initState :: AppConfig -> L.AppL AppState
initState cfg = do
  L.logInfo "Initializing KV DB..."
  eAstroKVDB <- L.initKVDB
    $ D.RocksDBConfig @KVDB.AstroDB "/tmp/hydra/catalogue" True False

  case eAstroKVDB of
    Left err -> do
      L.logError $ "Failed to init KV DB catalogue: " +|| err ||+ ""
      error $ "Failed to init KV DB catalogue: " +|| err ||+ ""    -- TODO
    Right astroKVDB -> do
      L.logInfo "KV DB initizlied."
      totalMeteors <- L.newVarIO 0
      pure $ AppState
        { _astroKVDB = astroKVDB
        , _totalMeteors = totalMeteors
        , _config = cfg
        }


doOrFail' :: Show e => (Text -> AppException) -> L.AppL (Either e a) -> L.AppL a
doOrFail' excF act = act >>= \case
  Left e  -> error $ show e
  Right a -> pure a

doOrFail :: Show e => L.AppL (Either e a) -> L.AppL a
doOrFail = doOrFail' OperationFailedException

connectOrFail :: D.DBConfig BS.SqliteM -> L.AppL (D.SqlConn BS.SqliteM)
connectOrFail cfg = doOrFail' ConnectionFailedException $ L.getOrInitSqlConn cfg


getMeteors :: Maybe Int -> Maybe Int -> D.SqlConn BS.SqliteM -> L.AppL Meteors
getMeteors mbMass mbSize conn = do
  L.logInfo $ "Lookup meteors with mbMass and mbSize: " <> show (mbMass, mbSize)

  let predicate meteorDB = case (mbMass, mbSize) of
        (Just m, Just s)  -> (SqlDB._meteorSize meteorDB ==. B.val_ s)
            &&. (SqlDB._meteorMass meteorDB ==. B.val_ m)
        (Just m, Nothing) -> (SqlDB._meteorMass meteorDB ==. B.val_ m)
        (Nothing, Just s) -> (SqlDB._meteorSize meteorDB ==. B.val_ s)
        _                 -> B.val_ True

  eRows <- L.scenario
    $ L.runDB conn
    $ L.findRows
    $ B.select
    $ B.filter_ predicate
    $ B.all_ (SqlDB._meteors SqlDB.astroDb)
  case eRows of
    Right ms -> pure $ Meteors $ map SqlDB.fromDBMeteor ms
    Left err -> do
      L.logError $ "Error occured on searching meteors: " <> show err
      pure $ Meteors []

createMeteor :: MeteorTemplate -> D.SqlConn BS.SqliteM -> L.AppL MeteorID
createMeteor mtp@(MeteorTemplate {..}) conn = do
  L.logInfo $ "Inserting meteor into SQL DB: " <> show mtp

  -- TODO: Proper time handling
  let time = Time.UTCTime (toEnum 1) (Time.secondsToDiffTime 0)

  doOrFail
    $ L.scenario
    $ L.runDB conn
    $ L.insertRows
    $ B.insert (SqlDB._meteors SqlDB.astroDb)
    $ B.insertExpressions
          [ SqlDB.Meteor B.default_
            (B.val_ size)
            (B.val_ mass)
            (B.val_ azimuth)
            (B.val_ altitude)
            (B.val_ time)
          ]

  let predicate meteorDB
          = (SqlDB._meteorSize meteorDB     ==. B.val_ size)
        &&. (SqlDB._meteorMass meteorDB     ==. B.val_ mass)
        &&. (SqlDB._meteorAzimuth meteorDB  ==. B.val_ azimuth)
        &&. (SqlDB._meteorAltitude meteorDB ==. B.val_ altitude)

  m <- doOrFail
    $ L.scenario
    $ L.runDB conn
    $ L.findRow
    $ B.select
    $ B.limit_ 1
    $ B.filter_ predicate
    $ B.all_ (SqlDB._meteors SqlDB.astroDb)
  pure $ SqlDB._meteorId $ fromJust m

-- TODO: move into the framework.
withDB
  :: D.DBConfig BS.SqliteM
  -> (D.SqlConn BS.SqliteM -> L.AppL a)
  -> L.AppL a
withDB cfg act = connectOrFail cfg >>= act
