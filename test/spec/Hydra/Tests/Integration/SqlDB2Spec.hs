{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PackageImports         #-}
{-# LANGUAGE TemplateHaskell        #-}

module Hydra.Tests.Integration.SqlDB2Spec where

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
