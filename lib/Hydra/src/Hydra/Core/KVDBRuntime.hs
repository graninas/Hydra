module Hydra.Core.KVDBRuntime where

import           Hydra.Prelude

import qualified Data.Map                        as Map

import qualified Hydra.Core.Domain               as D

import qualified Database.RocksDB as Rocks
import qualified Database.Redis as Redis

type RocksDBHandle    = MVar Rocks.DB
type RocksDBHandles   = TMVar (Map D.DBName RocksDBHandle)
type RedisConnections = TMVar (Map D.DBName Redis.Connection)

initRocksOptions :: Bool -> Bool -> Rocks.Options
initRocksOptions createIfMiss errorIfErr = Rocks.defaultOptions
  { Rocks.createIfMissing = createIfMiss
  , Rocks.errorIfExists   = errorIfErr
  }

initRocksDB'
  :: forall db
   . D.DB db
  => RocksDBHandles
  -> D.KVDBConfig db
  -> String
  -> IO (D.DBResult (D.DBHandle db))
initRocksDB' rocksDBsVars cfg@(D.RocksDBConfig _ createIfMiss errorIfErr) dbname = do
  let dbPath = D.getKVDBName cfg
  eDb <- try $ Rocks.open dbPath $ initRocksOptions createIfMiss errorIfErr
  case eDb of
    Left (err :: SomeException) -> pure $ Left $ D.DBError D.SystemError $ show err
    Right db -> do
      dbM <- newMVar db
      atomically $ do
        rocksDBs <- takeTMVar rocksDBsVars
        putTMVar rocksDBsVars $ Map.insert dbname dbM rocksDBs
      pure $ Right $ D.DBHandle D.RocksDB dbname
initRocksDB' _ _ _ = error "Invalid config."

-- TODO: defaultConnectInfo
initRedisDB'
  :: forall db
   . D.DB db
  => RedisConnections
  -> D.KVDBConfig db
  -> String
  -> IO (D.DBResult (D.DBHandle db))
initRedisDB' redisConnsVar _ dbname = do
  eConn <- try $ Redis.checkedConnect Redis.defaultConnectInfo
  case eConn of
    Left (err :: SomeException) -> pure $ Left $ D.DBError D.SystemError $ show err
    Right conn -> do
      atomically $ do
        mConns <- takeTMVar redisConnsVar
        putTMVar redisConnsVar $ Map.insert dbname conn mConns
      pure $ Right $ D.DBHandle D.Redis dbname

deInitRocksDB :: RocksDBHandle -> IO ()
deInitRocksDB rocksDBVar = do
  rocksDB <- takeMVar rocksDBVar
  Rocks.close rocksDB
  putMVar rocksDBVar rocksDB

closeRocksDBs :: RocksDBHandles -> IO ()
closeRocksDBs handleMapVar = do
  handleMap <- atomically $ takeTMVar handleMapVar
  mapM_ deInitRocksDB $ Map.elems handleMap
  atomically $ putTMVar handleMapVar Map.empty

closeRedisConns :: RedisConnections -> IO ()
closeRedisConns connsMapVar = do
  connsMap <- atomically $ takeTMVar connsMapVar
  mapM_ Redis.disconnect $ Map.elems connsMap
  atomically $ putTMVar connsMapVar Map.empty
