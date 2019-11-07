{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PackageImports         #-}
{-# LANGUAGE TemplateHaskell        #-}

module Hydra.Tests.Integration.SqlDBSpec where

import           Hydra.Prelude

import qualified Hydra.Domain                      as D
import qualified Hydra.Language                    as L
import           Hydra.Testing.Integrational
import           Hydra.Testing.Wrappers
import           Test.Hspec
import           Database.Beam
import qualified Database.Beam as B
import qualified Database.Beam.Query as B
import qualified Database.Beam.Sqlite as BS
import           Database.Beam.Sqlite (Sqlite)
import qualified Database.Beam.Sqlite as SQLite
import qualified Database.SQLite.Simple as SQLite (Connection)
import           Database.Beam.Query (runSelectReturningList)

import           Hydra.TestData
import           Hydra.TestData.Types.Meteor
import qualified Hydra.TestData.Types.SqlDB.CatalogueDB as CatDB
import           Hydra.Tests.Integration.Common

connectOrFail :: D.DBConfig beM -> L.AppL (D.SqlConn beM)
connectOrFail cfg = L.initSqlDB cfg >>= \case
    Left e     -> error $ show e
    Right conn -> pure conn

-- runBeamSqliteDebug putStrLn {- for debug output -} conn $ runInsert $
-- insert (_shoppingCartUsers shoppingCartDb) $
-- insertValues [ User "james@example.com" "James" "Smith" "b4cc344d25a2efe540adbf2678e2304c" {- james -}
--              , User "betty@example.com" "Betty" "Jones" "82b054bd83ffad9b6cf8bdb98ce3cc2f" {- betty -}
--              , User "sam@example.com" "Sam" "Taylor" "332532dcfaa1cbf61e2a266bd723612c" {- sam -} ]

-- let allUsers = all_ (_shoppingCartUsers shoppingCartDb)
--
-- runBeamSqliteDebug putStrLn conn $ do
--   users <- runSelectReturningList $ select allUsers
--   mapM_ (liftIO . putStrLn . show) users

-- SqlSelect be0 (QExprToIdentity (CatDB.DBMeteorT (QExpr be0 QBaseScope)))

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



sqliteCfg :: D.DBConfig BS.SqliteM
sqliteCfg = D.mkSQLiteConfig2 "test.db"

dbApp :: L.AppL (Either String (Maybe Meteor))
dbApp = do
  conn <- connectOrFail sqliteCfg
  eRes <- L.scenario
        $ L.evalSqlDB conn
        $ L.findRow
        $ getMeteorsWithMass 100
  case eRes of
        Left err -> pure $ Left $ show err
        Right res -> pure $ Right $ fmap convertMeteor res

spec :: Spec
spec = do

  unstableTest $ fastTest $ describe "SQLite DB tests 2" $ do

    describe "Some SQLite DB & Beam test 2" $ do
      it "Simple queries" $ do
        eRes <- evalApp dbApp
        eRes `shouldBe` Right Nothing
