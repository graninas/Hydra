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
import qualified Hydra.TestData.Types.SqlDB.CatalogueDB as SqlDB

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



dbApp :: D.SqlDBConfig be -> L.AppL (Either String ())
dbApp cfg = do
  eDB <- L.initSqlDB cfg
  case eDB of
    Left err -> pure $ Left $ show err
    Right db -> do
      meteors <- runQuery $ select (all_ (SqlDB._meteors SqlDB.catalogueDB))
      pure $ Right ()


spec :: Spec
spec = do

  unstableTest $ fastTest $ describe "SQLite DB tests" $ do
    let cfg = D.mkSQLiteConfig "test_db"

    describe "Some SQLite DB & Beam test" $ do
      it "DB is missing, create, errorIfExists False, no errors expected" $ do
        eRes <- evalApp $ dbInitApp cfg
        eRes `shouldBe` Right ()
