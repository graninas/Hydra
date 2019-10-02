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

import           Hydra.TestData
import           Hydra.TestData.Types.Meteor
import qualified Hydra.TestData.Types.SqlDB.CatalogueDB as CatDB
import           Database.Beam.Sqlite (Sqlite)

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

convertMeteor :: CatDB.Meteor -> Meteor
convertMeteor m = Meteor
  { _id        = CatDB._id m
  , _size      = CatDB._size m
  , _mass      = CatDB._mass m
  , _coords    = Coords (CatDB._azimuth m) (CatDB._altitude m)
  , _timestamp = CatDB._timestamp m
  }

dbApp :: D.SQLiteConfig -> L.AppL (Either String [Meteor])
dbApp cfg = do
  eDB <- L.initSQLiteDB cfg
  case eDB of
    Left err -> pure $ Left $ show err
    Right db -> do
      eMeteors <- L.scenario
        $ L.evalSQLiteDB db
        $ L.runBeamSelect
        $ select (all_ (CatDB._meteors CatDB.catalogueDB))
      case eMeteors of
        Right ms -> pure $ Right $ map convertMeteor ms
        Left err -> pure $ Left $ show err

spec :: Spec
spec = do

  unstableTest $ fastTest $ describe "SQLite DB tests" $ do
    let cfg = D.mkSQLiteConfig "test.db"

    describe "Some SQLite DB & Beam test" $ do
      it "Simple queries" $ do
        eRes <- evalApp $ dbApp cfg
        eRes `shouldBe` Right []
