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

-- data AppData = AppData
--     { _kBlocksDB     :: D.Storage KBlocksDB
--     , _kBlocksMetaDB :: D.Storage KBlocksMetaDB
--     }
--
-- makeFieldsNoPrefix ''AppData
--
-- putKBlockMetaApp :: D.KBlock -> D.DBConfig KBlocksMetaDB -> L.AppDefinitionL (Either D.DBError ())
-- putKBlockMetaApp kBlock cfg = do
--     let k = D.toDBKey   @KBlockMetaEntity kBlock
--     let v = D.toDBValue @KBlockMetaEntity kBlock
--     eDB <- L.scenario $ L.initDatabase cfg
--     case eDB of
--         Left err -> pure $ Left err
--         Right db -> L.scenario
--             $ L.withDatabase db
--             $ L.putEntity k v
--
-- getKBlockMetaApp :: D.DBKey KBlockMetaEntity -> D.DBConfig KBlocksMetaDB -> L.AppDefinitionL (Either D.DBError (D.DBValue KBlockMetaEntity))
-- getKBlockMetaApp k cfg = do
--     eDB <- L.scenario $ L.initDatabase cfg
--     case eDB of
--         Left err -> pure $ Left err
--         Right db -> L.scenario $ L.withDatabase db $ L.getValue k
--
-- putGetKBlockMetaApp :: FilePath -> L.AppDefinitionL (Either D.DBError Bool)
-- putGetKBlockMetaApp dbPath = do
--     let dbOpts = D.DBOptions
--             { D._createIfMissing = True
--             , D._errorIfExists   = True
--             }
--     let cfg :: D.DBConfig KBlocksMetaDB = D.DBConfig dbPath dbOpts
--     eDB <- L.scenario $ L.initDatabase cfg
--     case eDB of
--         Left err -> pure $ Left err
--         Right db -> L.scenario $ L.withDatabase db $ do
--             eRes <- L.putEntity kBlock1MetaKey kBlock1MetaValue
--             case eRes of
--                 Left err -> pure $ Left err
--                 Right _  -> do
--                     eVal <- L.getValue kBlock1MetaKey
--                     pure $ eVal >>= (\val2 -> Right (kBlock1MetaValue == val2))
--
-- kBlock1 :: D.KBlock
-- kBlock1 = D.KBlock
--     { D._time      = 0
--     , D._prevHash  = D.genesisHash
--     , D._number    = 1
--     , D._nonce     = 0
--     , D._solver    = D.genesisHash
--     }
--
-- kBlock2 :: D.KBlock
-- kBlock2 = D.KBlock
--     { D._time      = 1
--     , D._prevHash  = D.toHash kBlock1
--     , D._number    = 2
--     , D._nonce     = 2
--     , D._solver    = D.genesisHash
--     }
--
-- kBlock3 :: D.KBlock
-- kBlock3 = D.KBlock
--     { D._time      = 3
--     , D._prevHash  = D.toHash kBlock2
--     , D._number    = 3
--     , D._nonce     = 3
--     , D._solver    = D.genesisHash
--     }
--
-- kBlock1MetaKey :: D.DBKey KBlockMetaEntity
-- kBlock1MetaKey = D.toDBKey kBlock1
--
-- kBlock1MetaValue :: D.DBValue KBlockMetaEntity
-- kBlock1MetaValue = D.toDBValue kBlock1
--
-- kBlock2MetaKey :: D.DBKey KBlockMetaEntity
-- kBlock2MetaKey = D.toDBKey kBlock2
--
-- kBlock2MetaValue :: D.DBValue KBlockMetaEntity
-- kBlock2MetaValue = D.toDBValue kBlock2
--
-- kBlock3MetaKey :: D.DBKey KBlockMetaEntity
-- kBlock3MetaKey = D.toDBKey kBlock3
--
-- kBlock3MetaValue :: D.DBValue KBlockMetaEntity
-- kBlock3MetaValue = D.toDBValue kBlock3

dbInitApp :: forall db. D.DB db => D.KVDBConfig db -> L.AppL (D.DBResult ())
dbInitApp cfg = do
  eDB <- L.scenario $ L.initKVDB cfg
  pure $ eDB >> Right ()
--
-- withCatalogueDB :: AppState -> L.KVDBL CatalogueDB a -> L.LangL a
-- withCatalogueDB st = L.withKVDB (st ^. catalogueDB)


spec :: Spec
spec = stableTest $ fastTest $ describe "KV DB tests" $ do
    dbPath <- runIO $ mkDbPath "test"
    let cfg1 = D.KVDBConfig dbPath $ D.KVDBOptions
                { D._createIfMissing = True
                , D._errorIfExists   = False
                }
    let cfg2 = D.KVDBConfig dbPath $ D.KVDBOptions
                { D._createIfMissing = True
                , D._errorIfExists   = True
                }

    -- describe "DB Entities tests" $ do
    --     it "ToDBKey test" $
    --         kBlock1MetaKey   `shouldBe` KBlockMetaKey (kBlock1 ^. Lens.prevHash)
    --
    --     it "ToDBValue test" $
    --         kBlock1MetaValue `shouldBe` KBlockMetaValue 1
    --
    --     it "RawDBEntity test" $
    --         D.toRawDBKey @KBlocksMetaDB kBlock1MetaKey `shouldBe` D.fromStringHash (kBlock1 ^. Lens.prevHash)
    --
    --     it "Parse RawDBValue test" $ do
    --         let dbValueRaw = D.toRawDBValue @KBlocksMetaDB kBlock1MetaValue
    --         D.fromRawDBValue @KBlocksMetaDB dbValueRaw `shouldBe` Just kBlock1MetaValue
    --
    --     it "Different objects => different keys and values" $ do
    --         kBlock1MetaKey   `shouldNotBe` kBlock2MetaKey
    --         kBlock1MetaValue `shouldNotBe` kBlock2MetaValue
    --         D.toRawDBKey   @KBlocksMetaDB kBlock1MetaKey   `shouldNotBe` D.toRawDBKey   @KBlocksMetaDB kBlock2MetaKey
    --         D.toRawDBValue @KBlocksMetaDB kBlock1MetaValue `shouldNotBe` D.toRawDBValue @KBlocksMetaDB kBlock2MetaValue
    --
    describe "Database creation tests" $ do
        it "DB is missing, create, errorIfExists False, no errors expected" $ withDbAbsence dbPath $ do
            eRes <- evalApp $ dbInitApp @CatalogueDB cfg1
            eRes `shouldBe` Right ()

        it "DB is missing, create, errorIfExists True, no errors expected" $ withDbAbsence dbPath $ do
            eRes <- evalApp $ dbInitApp @CatalogueDB cfg2
            eRes `shouldBe` Right ()

        it "DB is present, create, errorIfExists False, no errors expected" $ withDbPresence dbPath $ do
            eRes <- evalApp $ dbInitApp @CatalogueDB cfg1
            eRes `shouldBe` Right ()

        it "DB is present, create, errorIfExists False, errors expected" $ withDbPresence dbPath $ do
            eRes <- evalApp $ dbInitApp @CatalogueDB cfg2
            eRes `shouldBe` Left (D.DBError D.SystemError ("user error (open: Invalid argument: " +| dbPath |+ ": exists (error_if_exists is true))"))

    -- describe "Database usage tests" $ do
    --     it "Write and Read KBlock Meta" $ withDbAbsence dbPath $ do
    --         eRes <- evalApp $ putGetKBlockMetaApp dbPath
    --         eRes `shouldBe` Right True
    --
    --     it "Write and Read KBlock1 Meta in separate runs" $ withDbAbsence dbPath $ do
    --         eInitialized <- evalApp $ dbInitApp cfg1
    --         eInitialized `shouldBe` Right ()
    --
    --         eStoreResult <- evalApp $ putKBlockMetaApp kBlock1 cfg1
    --         eStoreResult `shouldBe` Right ()
    --
    --         eValue <- evalApp $ getKBlockMetaApp kBlock1MetaKey cfg1
    --         eValue `shouldBe` Right kBlock1MetaValue
    --
    --     it "Write and Read KBlock2 Meta in separate runs" $ withDbAbsence dbPath $ do
    --         eInitialized <- evalApp $ dbInitApp cfg1
    --         eInitialized `shouldBe` Right ()
    --
    --         eStoreResult <- evalApp $ putKBlockMetaApp kBlock2 cfg1
    --         eStoreResult `shouldBe` Right ()
    --
    --         eValue <- evalApp $ getKBlockMetaApp kBlock2MetaKey cfg1
    --         eValue `shouldBe` Right kBlock2MetaValue
    --
    --     it "Write and Read KBlock3 Meta in separate runs" $ withDbAbsence dbPath $ do
    --         eInitialized <- evalApp $ dbInitApp cfg1
    --         eInitialized `shouldBe` Right ()
    --
    --         eStoreResult <- evalApp $ putKBlockMetaApp kBlock3 cfg1
    --         eStoreResult `shouldBe` Right ()
    --
    --         eValue <- evalApp $ getKBlockMetaApp kBlock3MetaKey cfg1
    --         eValue `shouldBe` Right kBlock3MetaValue
    --
    --     it "Read unexisting KBlock Meta" $ withDbPresence dbPath $ do
    --         eInitialized <- evalApp $ dbInitApp cfg1
    --         eInitialized `shouldBe` Right ()
    --
    --         eValue <- evalApp $ getKBlockMetaApp kBlock1MetaKey cfg1
    --         eValue `shouldBe` Left (D.DBError D.KeyNotFound (show $ D.fromStringHash $ kBlock1 ^. Lens.prevHash))
    --
    --     it "Write one, read another (unexisting) KBlock Meta" $ withDbPresence dbPath $ do
    --         eInitialized <- evalApp $ dbInitApp cfg1
    --         eInitialized `shouldBe` Right ()
    --
    --         eStoreResult <- evalApp $ putKBlockMetaApp kBlock1 cfg1
    --         eStoreResult `shouldBe` Right ()
    --
    --         eValue <- evalApp $ getKBlockMetaApp kBlock2MetaKey cfg1
    --         eValue `shouldBe` Left (D.DBError D.KeyNotFound (show $ D.fromStringHash $ kBlock2 ^. Lens.prevHash))
    --
    --     it "Write two entities, read both" $ withDbPresence dbPath $ do
    --         eInitialized <- evalApp $ dbInitApp cfg1
    --         eInitialized `shouldBe` Right ()
    --
    --         eStoreResult1 <- evalApp $ putKBlockMetaApp kBlock1 cfg1
    --         eStoreResult1 `shouldBe` Right ()
    --
    --         eStoreResult2 <- evalApp $ putKBlockMetaApp kBlock2 cfg1
    --         eStoreResult2 `shouldBe` Right ()
    --
    --         eValue1 <- evalApp $ getKBlockMetaApp kBlock1MetaKey cfg1
    --         eValue1 `shouldBe` Right kBlock1MetaValue
    --
    --         eValue2 <- evalApp $ getKBlockMetaApp kBlock2MetaKey cfg1
    --         eValue2 `shouldBe` Right kBlock2MetaValue
