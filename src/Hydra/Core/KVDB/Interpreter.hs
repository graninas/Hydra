module Hydra.Core.KVDB.Interpreter where

import           Hydra.Prelude

import qualified Hydra.Core.Language as L
import qualified Hydra.Core.RLens    as RLens
import qualified Hydra.Core.Runtime  as R
import qualified Hydra.Core.Domain   as D

import qualified Database.RocksDB as Rocks

writeOpts :: Rocks.WriteOptions
writeOpts = Rocks.defaultWriteOptions
  { Rocks.sync = True
  }

interpretKVDBF :: Rocks.DB -> L.KVDBF a -> IO a
interpretKVDBF db (L.Load key next) = do
  mbVal <- Rocks.get db Rocks.defaultReadOptions key
  pure $ next $ case mbVal of
    Nothing  -> Left $ D.DBError D.KeyNotFound $ show key
    Just val -> Right val
interpretKVDBF db (L.Save key val next) =
  next . Right <$> Rocks.put db writeOpts key val

runKVDBL :: R.RocksDBHandle -> L.KVDBL db a -> IO a
runKVDBL handle act = do
  db <- takeMVar handle
  res <- foldFree (interpretKVDBF db) act
  putMVar handle db
  pure res
