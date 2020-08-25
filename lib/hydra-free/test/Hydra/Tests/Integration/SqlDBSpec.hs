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

import qualified Hydra.Domain   as D
import qualified Hydra.Language as L
import qualified "hydra-base" Hydra.Runtime  as R
import           Hydra.Testing.Integrational
import           Hydra.Testing.Wrappers
import           Hydra.TestData
import           Hydra.TestData.Types.Meteor
import qualified Hydra.TestData.Types.SqlDB.CatalogueDB as CatDB
import           Hydra.Tests.Integration.Common



-- runBeamSqliteDebug putStrLn {- for debug output -} conn $ runInsert $
-- insert (_shoppingCartUsers shoppingCartDb) $
-- insertValues [ User "james@example.com" "James" "Smith" "b4cc344d25a2efe540adbf2678e2304c" {- james -}
--              , User "betty@example.com" "Betty" "Jones" "82b054bd83ffad9b6cf8bdb98ce3cc2f" {- betty -}
--              , User "sam@example.com" "Sam" "Taylor" "332532dcfaa1cbf61e2a266bd723612c" {- sam -} ]
--
-- let allUsers = all_ (_shoppingCartUsers shoppingCartDb)

-- runBeamSqliteDebug putStrLn conn $ do
--   users <- runSelectReturningList $ select allUsers
--   mapM_ (liftIO . putStrLn . show) users
--
-- SqlSelect be0 (QExprToIdentity (CatDB.DBMeteorT (QExpr be0 QBaseScope)))
--
-- getMeteorsWithMass :: D.SQLiteHandle -> Int -> L.AppL [CatDB.DBMeteor]
-- getMeteorsWithMass sqliteConn size = do
--   eMeteors <- L.scenario
--     $ L.evalSQLiteDB sqliteConn
--     $ L.runBeamSelect
--     $ B.select
--     $ B.filter_ (\meteor -> CatDB._size meteor ==. B.val_ size)
--     $ B.all_ (CatDB._meteors CatDB.catalogueDB)
--   case eMeteors of
--     Left err -> do
--       L.logError $ "Error occurred when extracting meteors: " <> show err
--       pure []
--     Right ms -> pure ms

data SqlDBException = SqlDBException Text
  deriving (Show, Eq)

instance Exception SqlDBException

connectOrFail :: D.DBConfig SQLite.SqliteM -> L.AppL (D.SqlConn SQLite.SqliteM)
connectOrFail cfg = L.initSqlDB cfg >>= \case
  Left err   -> L.scenario $ L.throwException $ SqlDBException $ show err
  Right conn -> pure conn

withTestDB :: ((R.AppRuntime, D.SqlConn SQLite.SqliteM) -> IO a) -> IO a
withTestDB act = R.withAppRuntime Nothing $ \rt -> do
    conn <- R.startApp rt $ connectOrFail sqliteCfg
    act (rt, conn)
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

findMeteor :: D.SqlConn SQLite.SqliteM -> L.AppL (Either SqlDBException (Maybe Meteor))
findMeteor conn = L.scenario $ L.runSafely $ do
  mbRow <- getRow conn $ getMeteorsWithMass 100
  pure $ convertMeteor <$> mbRow


-- insertMeteor :: L.AppL (Either SomeException ())
-- insertMeteor = L.runSafely $ do
--   conn <- connectOrFail sqliteCfg

  -- around (R.withAppRuntime Nothing) $
  --   describe "KV DB functional tests" $
  --     it "load game failure test" $ \appRt -> do



spec :: Spec
spec = around withTestDB $
  describe "SQLite DB tests" $
    it "Select, not found" $ \(rt, conn) -> do
      eRes <- R.startApp rt $ findMeteor conn
      eRes `shouldBe` Right Nothing

    --   it "Insert / Select / Delete test" $ do
    --     eRes <- evalApp insertSelectApp
    --     eRes `shouldBe` Right Nothing
