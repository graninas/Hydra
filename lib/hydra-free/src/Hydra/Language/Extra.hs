{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Hydra.Language.Extra where

import           Hydra.Prelude

import qualified Hydra.Core.Language      as L
import qualified Hydra.Domain             as D
import qualified Hydra.Framework.Language as L


-- | Get existing SQL connection, or init a new connection.
getOrInitSqlConn :: D.DBConfig beM -> L.AppL (D.DBResult (D.SqlConn beM))
getOrInitSqlConn cfg = do
  eConn <- L.scenario $ L.getSqlDBConnection cfg
  case eConn of
    Left (D.DBError D.ConnectionDoesNotExist _) -> L.initSqlDB cfg
    res                                         -> pure res

-- TODO: rework it
foreverApp :: L.AppL a -> L.AppL ()
foreverApp app = do
  _ <- app
  awaitVar <- L.newVarIO (1 :: Int)
  L.process $ do
    L.delay 10000000000
    L.writeVarIO awaitVar 1
  L.atomically $ do
    x <- L.readVar awaitVar
    when (x == 1) L.retry

awaitAppForever :: L.AppL ()
awaitAppForever = L.atomically $ do
    xVar <- L.newVar (1 :: Int)
    x <- L.readVar xVar
    when (x == 1) L.retry
