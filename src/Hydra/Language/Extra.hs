{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Hydra.Language.Extra where

import           Hydra.Prelude

import qualified Hydra.Core.Language      as L
import qualified Hydra.Core.ChurchL      as CL
import qualified Hydra.Domain             as D
import qualified Hydra.Framework.Language as L
import qualified Hydra.Framework.ChurchL as L

class App m => ForeverApp m where
  foreverApp :: m a -> m ()

instance ForeverApp L.LangL where
  foreverApp app = do
    app

    awaitVar <- L.newVarIO (1 :: Int)
    L.process $ do
      L.delay 10000000000
      L.writeVarIO awaitVar 1
    L.atomically $ do
      x <- L.readVar awaitVar
      when (x == 1) L.retry

instance ForeverApp CL.LangL where
  foreverApp :: CL.AppL a -> CL.AppL ()
  foreverApp app = do
    app

    awaitVar <- CL.newVarIO (1 :: Int)
    CL.process $ do
      CL.delay 10000000000
      CL.writeVarIO awaitVar 1
    CL.atomically $ do
      x <- CL.readVar awaitVar
      when (x == 1) CL.retry
