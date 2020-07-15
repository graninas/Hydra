{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PackageImports         #-}
{-# LANGUAGE TemplateHaskell        #-}

module Hydra.Tests.Integration.KVDBSpec where

import           Hydra.Prelude

import qualified Hydra.Domain                      as D
import qualified Hydra.Language                    as L
import           Hydra.Testing.Integrational
import           Hydra.Testing.Wrappers
import           Test.Hspec

import           Hydra.TestData

dbInitApp :: forall db. D.DB db => D.KVDBConfig db -> L.AppL (D.DBResult ())
dbInitApp cfg = do
  eDB <- L.initKVDB cfg
  pure $ eDB >> Right ()


spec :: Spec
spec = do
  -- unstableTest $ fastTest $ describe "Redis KV DB tests" $ do
  --   dbTestPath <- runIO $ mkTestPath "db_test"
  --   let cfg1 = D.RedisConfig @CatalogueDB
  --   let cfg2 = D.RedisConfig @CatalogueDB
  --   let kvdbPath1 = D.getKVDBName cfg1
  --   let kvdbPath2 = D.getKVDBName cfg2
  --
  --   describe "Database creation tests" $ do
  --       it "DB is missing, create, errorIfExists False, no errors expected" $ withRedisDbAbsence kvdbPath1 $ do
  --           eRes <- evalApp $ dbInitApp cfg1
  --           eRes `shouldBe` Right ()
  --
  --       it "DB is missing, create, errorIfExists True, no errors expected" $ withRedisDbAbsence kvdbPath2 $ do
  --           eRes <- evalApp $ dbInitApp cfg2
  --           eRes `shouldBe` Right ()
  --
  --       it "DB is present, create, errorIfExists False, no errors expected" $ withRedisDbPresence kvdbPath1 $ do
  --           eRes <- evalApp $ dbInitApp cfg1
  --           eRes `shouldBe` Right ()
  --
  --       it "DB is present, create, errorIfExists False, errors expected" $ withRedisDbPresence kvdbPath2 $ do
  --           eRes <- evalApp $ dbInitApp cfg2
  --           eRes `shouldBe` Left (D.DBError D.SystemError ("user error (open: Invalid argument: "
  --             +| kvdbPath2 |+ ": exists (error_if_exists is true))"))


  unstableTest $ fastTest $ describe "Rocks KV DB tests" $ do
    dbTestPath <- runIO $ mkTestPath "db_test"
    let cfg1 = D.RocksDBConfig @CatalogueDB dbTestPath True False
    let cfg2 = D.RocksDBConfig @CatalogueDB dbTestPath True True
    let kvdbPath1 = D.getKVDBName cfg1
    let kvdbPath2 = D.getKVDBName cfg2

    describe "Database creation tests" $ do
        it "DB is missing, create, errorIfExists False, no errors expected" $ withRocksDbAbsence kvdbPath1 $ do
            eRes <- evalApp $ dbInitApp cfg1
            eRes `shouldBe` Right ()

        it "DB is missing, create, errorIfExists True, no errors expected" $ withRocksDbAbsence kvdbPath2 $ do
            eRes <- evalApp $ dbInitApp cfg2
            eRes `shouldBe` Right ()

        it "DB is present, create, errorIfExists False, no errors expected" $ withRocksDbPresence kvdbPath1 $ do
            eRes <- evalApp $ dbInitApp cfg1
            eRes `shouldBe` Right ()

        it "DB is present, create, errorIfExists False, errors expected" $ withRocksDbPresence kvdbPath2 $ do
            eRes <- evalApp $ dbInitApp cfg2
            eRes `shouldBe` Left (D.DBError D.SystemError ("user error (open: Invalid argument: "
              +| kvdbPath2 |+ ": exists (error_if_exists is true))"))
