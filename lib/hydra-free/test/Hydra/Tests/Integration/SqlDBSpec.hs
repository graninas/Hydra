{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PackageImports         #-}
{-# LANGUAGE TemplateHaskell        #-}

module Hydra.Tests.Integration.SqlDBSpec where

import           Hydra.Prelude

import           Test.Hspec
import           Database.Beam
import qualified Database.Beam as B
import qualified Database.Beam.Query as B
import qualified Database.Beam.Sqlite as BS
import           Database.Beam.Sqlite (Sqlite)
import qualified Database.Beam.Sqlite as SQLite
import qualified Database.SQLite.Simple as SQLite (Connection)
import           Database.Beam.Query (runSelectReturningList)
import           Data.Time.Clock (UTCTime(..), secondsToDiffTime)

import qualified Hydra.Domain   as D
import qualified Hydra.Language as L
import qualified "hydra-base" Hydra.Runtime  as R
import           Hydra.Testing.Integrational
import           Hydra.Testing.Wrappers
import           Hydra.TestData
import           Hydra.TestData.Types.Meteor
import qualified Hydra.TestData.Types.SqlDB.CatalogueDB as CatDB
import           Hydra.Tests.Integration.Common



data SqlDBException = SqlDBException Text
  deriving (Show, Eq)

instance Exception SqlDBException

connectOrFail :: D.DBConfig SQLite.SqliteM -> L.AppL (D.SqlConn SQLite.SqliteM)
connectOrFail cfg = L.initSqlDB cfg >>= \case
  Left err   -> L.scenario $ L.throwException $ SqlDBException $ show err
  Right conn -> pure conn

withTestDB :: ((R.AppRuntime, D.SqlConn SQLite.SqliteM) -> IO a) -> IO a
withTestDB act = R.withAppRuntime Nothing $ \rt -> do
    conn <- R.startApp rt $ do
      conn <- connectOrFail sqliteCfg
      void $ deleteAllMeteors conn
      pure conn
    a <- act (rt, conn)
    void $ R.startApp rt $ do
      deleteAllMeteors conn
      -- TODO: deinit conn here
    pure a
  where
    sqliteCfg :: D.DBConfig BS.SqliteM
    sqliteCfg = D.mkSQLiteConfig "test.db"

getRow
  :: ( D.BeamRunner SQLite.SqliteM
     , D.BeamRuntime be SQLite.SqliteM
     , B.FromBackendRow be a
     )
  => D.SqlConn SQLite.SqliteM
  -> B.SqlSelect be a
  -> L.LangL (Maybe a)
getRow conn query = do
  eRow <- L.evalSqlDB conn $ L.findRow query
  case eRow of
    Left err    -> L.throwException $ SqlDBException $ show err
    Right mbRow -> pure mbRow


findMeteor
  :: D.SqlConn SQLite.SqliteM
  -> L.AppL (Either SqlDBException (Maybe Meteor))
findMeteor conn = L.scenario $ L.runSafely $ do
  mbRow <- getRow conn $ getMeteorsWithMass 100
  pure $ convertMeteor <$> mbRow

getMeteor
  :: D.SqlConn SQLite.SqliteM
  -> Int32
  -> L.AppL (Either SqlDBException (Maybe Meteor))
getMeteor conn pkVal = L.scenario $ L.runSafely $ do
  mbRow <- getRow conn
    $ B.select
    $ B.filter_ (\meteor -> CatDB._id meteor ==. B.val_ pkVal)
    $ B.all_ (CatDB._meteors CatDB.catalogueDB)
  pure $ convertMeteor <$> mbRow

insertMeteorQuery
  :: Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> UTCTime
  -> L.SqlDBL SQLite.SqliteM ()
insertMeteorQuery meteorId size mass azmth alt time = do
  let meteorDB :: CatDB.DBMeteor = CatDB.DBMeteor
        { CatDB._id        = meteorId
        , CatDB._size      = size
        , CatDB._mass      = mass
        , CatDB._azimuth   = azmth
        , CatDB._altitude  = alt
        , CatDB._timestamp = time
        }
  L.insertRows
    $ B.insert (CatDB._meteors CatDB.catalogueDB)
    $ B.insertExpressions [ B.val_ meteorDB ]
       -- Sample of autoincrement:
       -- $ B.insertExpressions [ (B.val_ meteorDB) { CatDB._id = B.default_ } ]

insertMeteor :: D.SqlConn SQLite.SqliteM -> Int32 -> L.AppL (D.DBResult ())
insertMeteor conn meteorId
  = L.scenario
  $ L.evalSqlDB conn
  $ insertMeteorQuery meteorId 100 100 100 100 (UTCTime (toEnum 1) (secondsToDiffTime 0))


deleteMeteor :: D.SqlConn SQLite.SqliteM -> Int32 -> L.AppL (D.DBResult ())
deleteMeteor conn meteorId
  = L.scenario
  $ L.evalSqlDB conn
  $ L.deleteRows
  $ B.delete (CatDB._meteors CatDB.catalogueDB)
  $ (\m -> (CatDB._id m) ==. (B.val_ meteorId))

deleteAllMeteors :: D.SqlConn SQLite.SqliteM -> L.AppL (D.DBResult ())
deleteAllMeteors conn
  = L.scenario
  $ L.evalSqlDB conn
  $ L.deleteRows
  $ B.delete (CatDB._meteors CatDB.catalogueDB)
  $ (\m -> (CatDB._id m) /=. (B.val_ 0))


spec :: Spec
spec = around withTestDB $
  describe "SQLite DB tests" $ do
    it "Select, not found" $ \(rt, conn) -> do
      eRes <- R.startApp rt $ findMeteor conn
      eRes `shouldBe` Right Nothing

    it "Insert / Select / Delete test" $ \(rt, conn) -> do
      (eRes1, eRes2, eRes3, eRes4) <- R.startApp rt $ do
        eRes1 <- insertMeteor conn 100
        eRes2 <- getMeteor conn 100
        eRes3 <- deleteMeteor conn 100
        eRes4 <- getMeteor conn 100
        pure (eRes1, eRes2, eRes3, eRes4)

      let m = Meteor
              { _id        = 100
              , _size      = 100
              , _mass      = 100
              , _coords    = Coords 100 100
              , _timestamp = UTCTime (toEnum 1) (secondsToDiffTime 0)
              }
      isRight eRes1 `shouldBe` True
      eRes2 `shouldBe` (Right (Just m))
      isRight eRes3 `shouldBe` True
      eRes4 `shouldBe` (Right Nothing)

    it "Transaction test" $ \(rt, conn) -> do
      let timestamp = UTCTime (toEnum 1) (secondsToDiffTime 0)

      eRes1 <- R.startApp rt $ getMeteor conn 1

      eRes2 <- R.startApp rt
          $ L.scenario
          $ L.evalSqlDB conn $ do
            insertMeteorQuery 1 1 1 1 1 timestamp
            error "This is exception."

      eRes3 <- R.startApp rt $ getMeteor conn 1

      eRes1 `shouldBe` (Right Nothing)
      isLeft eRes2 `shouldBe` True
      eRes3 `shouldBe` (Right Nothing)
