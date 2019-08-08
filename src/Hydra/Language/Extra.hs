{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Hydra.Language.Extra where

import           Hydra.Prelude

import qualified Hydra.Core.ChurchL       as CL
import qualified Hydra.Core.Class         as C
import qualified Hydra.Core.Language      as L
import qualified Hydra.Domain             as D
import qualified Hydra.Framework.ChurchL  as CL
import qualified Hydra.Framework.Class    as C
import qualified Hydra.Framework.Language as L

-- TODO: this is awful
-- class (C.Logger l, C.Random r, C.ControlFlow cf, C.State' s,
--   C.Lang l r cf s lang, C.Process lang proc, C.App l r cf s lang proc m) =>
--   ForeverApp m where
--   foreverApp :: m a -> m ()

-- TODO: rework it
foreverAppFree :: L.AppL a -> L.AppL ()
foreverAppFree app = do
  app
  awaitVar <- L.newVarIO (1 :: Int)
  L.process $ do
    L.delay 10000000000
    L.writeVarIO awaitVar 1
  L.atomically $ do
    x <- L.readVar awaitVar
    when (x == 1) L.retry

-- TODO: rework it
foreverAppChurch :: CL.AppL a -> CL.AppL ()
foreverAppChurch app = do
  app
  awaitVar <- CL.newVarIO (1 :: Int)
  CL.process $ do
    CL.delay 10000000000
    CL.writeVarIO awaitVar 1
  CL.atomically $ do
    x <- CL.readVar awaitVar
    when (x == 1) CL.retry


awaitAppForever :: L.AppL ()
awaitAppForever = L.atomically $ do
    xVar <- L.newVar (1 :: Int)
    x <- L.readVar xVar
    when (x == 1) L.retry

--
-- instance ForeverApp CL.LangL where
--   foreverApp :: CL.AppL a -> CL.AppL ()
--   foreverApp app = do
--     app
--
--     awaitVar <- CL.newVarIO (1 :: Int)
--     CL.process $ do
--       CL.delay 10000000000
--       CL.writeVarIO awaitVar 1
--     CL.atomically $ do
--       x <- CL.readVar awaitVar
--       when (x == 1) CL.retry
