{-# LANGUAGE AllowAmbiguousTypes #-}

module Hydra.Core.KVDB.Language where

import           Hydra.Prelude

-- import           Data.Typeable               (typeOf)

import qualified Hydra.Core.Domain.DB   as D
import qualified Hydra.Core.Domain.KVDB as D

-- data KVDBF db a where
--     GetValue :: D.KVDBKey -> (D.DBResult D.KVDBValue -> next) -> KVDBF db next
--     PutValue :: D.KVDBKey -> D.KVDBValue -> (D.DBResult () -> next) -> KVDBF db next
--   deriving (Functor)
--
-- type KVDBL db = Free (KVDBF db)

data KVDBF a where
  GetValue :: D.KVDBKey -> KVDBF (D.DBResult D.KVDBValue)
  PutValue :: D.KVDBKey -> D.KVDBValue -> KVDBF (D.DBResult ())

type KVDBL db = KVDBF

getValue :: D.KVDBKey -> KVDBL db (D.DBResult D.KVDBValue)
getValue = GetValue

putValue :: D.KVDBKey -> D.KVDBValue -> KVDBL db (D.DBResult ())
putValue = PutValue

-- putEntity
--     :: forall entity db
--     .  D.RawDBEntity db entity
--     => D.DBKey entity
--     -> D.DBValue entity
--     -> KVDBL db (D.DBResult ())
-- putEntity dbKey dbVal = let
--     rawKey = D.toRawDBKey   @db dbKey
--     rawVal = D.toRawDBValue @db dbVal
--     in putValue rawKey rawVal
--
-- -- | Puts a typed entity to the corresponding DB.
-- putEntity'
--     :: forall entity db src
--     .  D.RawDBEntity db entity
--     => D.ToDBKey   entity src
--     => D.ToDBValue entity src
--     => src
--     -> KVDBL db (D.DBResult ())
-- putEntity' src = let
--     rawKey = D.toRawDBKey   @db @entity $ D.toDBKey   src
--     rawVal = D.toRawDBValue @db @entity $ D.toDBValue src
--     in putValue rawKey rawVal
--
-- -- | Gets a typed entity from the corresponding DB.
-- getEntity
--     :: forall entity db
--     . (FromJSON (D.DBValue entity), D.RawDBEntity db entity, Typeable (D.DBValue entity))
--     => D.DBKey entity
--     -> KVDBL db (D.DBResult (D.DBE entity))
-- getEntity dbKey = do
--     let rawKey = D.toRawDBKey @db dbKey
--     let proxyVal = error "Don't call me, I'm Proxy" :: D.DBValue entity
--     eRawVal <- getValue rawKey
--     case eRawVal of
--         Left err       -> pure $ Left err
--         Right rawVal   -> case D.fromRawDBValue @db rawVal of
--             Nothing    -> pure $ Left $ D.DBError D.InvalidType ("Expected type: " <> show (typeOf proxyVal)
--                             <> ". Raw key: <" <> decodeUtf8 rawKey <>  ">. Raw data: <" <> decodeUtf8 rawVal <> ">")
--             Just dbVal -> pure $ Right (dbKey, dbVal)
--
-- -- | Gets a typed value from the corresponding DB.
-- getValue
--     :: (FromJSON (D.DBValue entity), D.RawDBEntity db entity, Typeable (D.DBValue entity))
--     => D.DBKey entity
--     -> KVDBL db (D.DBResult (D.DBValue entity))
-- getValue dbKey = do
--     eEntity <- getEntity dbKey
--     pure $ eEntity >>= Right . snd
--
-- -- | Gets a typed value from the corresponding DB.
-- getValue'
--     :: (FromJSON (D.DBValue entity), D.RawDBEntity db entity, Typeable (D.DBValue entity))
--     => D.ToDBKey entity src
--     => src
--     -> KVDBL db (D.DBResult (D.DBValue entity))
-- getValue' src = do
--     eEntity <- getEntity $ D.toDBKey src
--     pure $ eEntity >>= Right . snd
--
-- -- | Gets a typed value from the corresponding DB.
-- -- The difference from @getValue@ is that it forgets about DB errors.
-- findValue
--     :: (FromJSON (D.DBValue entity), D.RawDBEntity db entity, Typeable (D.DBValue entity))
--     => D.DBKey entity
--     -> KVDBL db (Maybe (D.DBValue entity))
-- findValue key = do
--     eVal <- getValue key
--     pure $ either (const Nothing) Just eVal
--
-- -- | Gets a typed value from the corresponding DB.
-- -- The difference from @getValue'@ is that it forgets about DB errors.
-- findValue'
--     :: (FromJSON (D.DBValue entity), D.RawDBEntity db entity, Typeable (D.DBValue entity))
--     => D.ToDBKey entity src
--     => src
--     -> KVDBL db (D.DBResult (Maybe (D.DBValue entity)))
-- findValue' src = do
--     eVal <- getValue' src
--     case eVal of
--         Left (D.DBError D.KeyNotFound _) -> pure $ Right Nothing
--         Left err                         -> pure $ Left err
--         Right val                        -> pure $ Right $ Just val
