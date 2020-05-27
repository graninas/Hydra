{-# LANGUAGE DeriveAnyClass #-}

module Labyrinth.KVDB.Repository where

import Labyrinth.Prelude
import Labyrinth.Types
import Labyrinth.KVDB.Model


withKVDB' :: DB db => KVDBConfig db -> KVDBL db a -> AppL a
withKVDB' cfg dbAct = do
  eConn <- initKVDB cfg
  scenario $ case eConn of
    Left err   -> throwException $ InvalidOperation $ show err
    Right conn -> withKVDB conn dbAct

loadGameState :: KVDBConfig LabKVDB -> Int -> AppL (DBResult GameEntity)
loadGameState kvdbCfg k =
  withKVDB' kvdbCfg $ loadEntity $ LabKey k

saveGameState :: KVDBConfig LabKVDB -> GameEntity -> AppL (DBResult ())
saveGameState kvdbCfg ge =
  withKVDB' kvdbCfg $ saveEntity @GameEntity @GameEntity @LabKVDB ge
