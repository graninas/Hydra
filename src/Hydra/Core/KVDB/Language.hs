{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Hydra.Core.KVDB.Language where

import           Hydra.Prelude

import qualified Hydra.Core.Domain.DB   as D
import qualified Hydra.Core.Domain.KVDB as D

import           Language.Haskell.TH.MakeFunctor (makeFunctorInstance)

data KVDBF next where
  Save :: D.KVDBKey -> D.KVDBValue -> (D.DBResult () -> next) -> KVDBF next
  Load :: D.KVDBKey -> (D.DBResult D.KVDBValue -> next) -> KVDBF next

makeFunctorInstance ''KVDBF

type KVDBL db = Free KVDBF

save' :: D.KVDBKey -> D.KVDBValue -> KVDBL db (D.DBResult ())
save' dbkey dbval = liftF $ Save dbkey dbval id

load' :: D.KVDBKey -> KVDBL db (D.DBResult D.KVDBValue)
load' dbkey = liftF $ Load dbkey id

saveEntity
  :: forall src entity db
   . D.DBEntity db entity
  => D.AsKeyEntity entity src
  => D.AsValueEntity entity src
  => src
  -> KVDBL db (D.DBResult ())
saveEntity src = save' dbkey dbval
  where
    k :: D.KeyEntity entity
    k = D.toKeyEntity src
    v :: D.ValueEntity entity
    v = D.toValueEntity src
    dbkey = D.toDBKey k
    dbval = D.toDBValue v

loadEntity
  :: forall entity dst db
   . D.DBEntity db entity
  => D.AsValueEntity entity dst
  => Show (D.KeyEntity entity)
  => D.KeyEntity entity
  -> KVDBL db (D.DBResult dst)
loadEntity key = do
  eRawVal <- load' (D.toDBKey key)
  pure $ case eRawVal of
    Left err  -> Left err
    Right val -> maybe (decodingErr val) (Right . D.fromValueEntity key) $ mbE val
  where
    mbE :: D.KVDBValue -> Maybe (D.ValueEntity entity)
    mbE = D.fromDBValue
    decodingErr val = Left
      $ D.DBError D.DecodingFailed
      $ "Failed to decode entity, k: "
          <> show key <> ", v: " <> show val
