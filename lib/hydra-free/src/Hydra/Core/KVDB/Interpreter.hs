module Hydra.Core.KVDB.Interpreter where

import           Hydra.Prelude

import qualified Hydra.Core.Language as L
import qualified Hydra.Core.KVDBRuntime as R

import qualified Database.RocksDB as Rocks
import qualified Database.Redis as Redis
import qualified Hydra.Core.KVDB.Impl.RocksDB as RocksDBImpl
import qualified Hydra.Core.KVDB.Impl.Redis as RedisImpl

interpretAsRocksDBF :: Rocks.DB -> L.KVDBF a -> IO a
interpretAsRocksDBF db (L.Load key next)     = next <$> RocksDBImpl.get db key
interpretAsRocksDBF db (L.Save key val next) = next <$> RocksDBImpl.put db key val

runAsRocksDBL :: R.RocksDBHandle -> L.KVDBL db a -> IO a
runAsRocksDBL handle act = do
  db <- takeMVar handle
  res <- foldFree (interpretAsRocksDBF db) act
  putMVar handle db
  pure res

interpretAsRedisF :: Redis.Connection -> L.KVDBF a -> IO a
interpretAsRedisF conn (L.Load key next)     = next <$> RedisImpl.get conn key
interpretAsRedisF conn (L.Save key val next) = next <$> RedisImpl.set conn key val

runAsRedisL :: Redis.Connection -> L.KVDBL db a -> IO a
runAsRedisL conn act = foldFree (interpretAsRedisF conn) act
