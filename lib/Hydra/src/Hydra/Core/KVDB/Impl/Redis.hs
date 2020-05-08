module Hydra.Core.KVDB.Impl.Redis where

import           Hydra.Prelude

import qualified Hydra.Core.Domain   as D

import qualified Database.Redis as Redis

-- set :: RedisCtx m f	=> ByteString	-> ByteString	-> m (f Status)
-- get :: RedisCtx m f => ByteString -> m (f (Maybe ByteString))

get :: Redis.Connection -> D.KVDBKey -> IO (D.DBResult D.KVDBValue)
get conn key = Redis.runRedis conn $ do
  fStatus <- Redis.get key
  case fStatus of
    Right (Just val)       -> pure $ Right val
    Right Nothing          -> pure $ Left $ D.DBError D.KeyNotFound $ decodeUtf8 key
    Left (Redis.Error err) -> pure $ Left $ D.DBError D.SystemError $ decodeUtf8 err
    res -> pure $ Left $ D.DBError D.UnknownDBError $ "Unhandled response from Redis: " +|| res ||+ "."

set :: Redis.Connection -> D.KVDBKey -> D.KVDBValue -> IO (D.DBResult ())
set conn key val = Redis.runRedis conn $ do
  fStatus <- Redis.set key val
  case fStatus of
    Right Redis.Ok -> pure $ Right ()
    Right (Redis.Status status) -> pure $ Left $ D.DBError D.SystemError $ decodeUtf8 status
    Left (Redis.Error err) -> pure $ Left $ D.DBError D.SystemError $ decodeUtf8 err
    res -> pure $ Left $ D.DBError D.UnknownDBError $ "Unhandled response from Redis: " +|| res ||+ "."
