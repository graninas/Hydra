{-# LANGUAGE DeriveAnyClass #-}

module Labyrinth.KVDB.Repository where

import           Labyrinth.Prelude
import           Labyrinth.Types
import           Labyrinth.KVDB.Model


withKVDB' :: DB db => KVDBConfig db -> KVDBL db a -> AppL a
withKVDB' cfg dbAct = do
  eConn <- initKVDB cfg
  scenario $ case eConn of
    Left err   -> throwException $ InvalidOperation $ show err
    Right conn -> withKVDB conn dbAct

loadLabyrinth :: KVDBConfig LabKVDB -> Int -> AppL GameInfo
loadLabyrinth kvdbCfg k = do
  eVal <- withKVDB' kvdbCfg $ loadEntity $ toKeyEntity k
  scenario $ case eVal of
    Left err  -> throwException $ InvalidOperation $ show err
    Right val -> pure val
  -- where
    -- 'Production' DB
    -- kvDBConfig = RocksDBConfig @LabKVDB "./prod/labyrinths"
-- 
-- saveLabyrinth :: KVDBConfig LabKVDB -> Int -> GameInfo -> AppL GameInfo
-- saveLabyrinth kvdbCfg k gi = do
--   eVal <- withKVDB' kvdbCfg $ saveEntity (toKeyEntity k)
--   scenario $ case eVal of
--     Left err  -> throwException $ InvalidOperation $ show err
--     Right val -> pure val
--   -- where
--     -- 'Production' DB
--     -- kvDBConfig = RocksDBConfig @LabKVDB "./prod/labyrinths"
