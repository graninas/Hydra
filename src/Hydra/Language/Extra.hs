{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Hydra.Language.Extra where

import           Hydra.Prelude

import qualified Hydra.Core.Language      as L
import qualified Hydra.Domain             as D
import qualified Hydra.Framework.Language as L



foreverApp :: L.AppL a -> L.AppL ()
foreverApp app = do
  app

  awaitVar <- L.newVarIO (1 :: Int)
  L.process $ do
    L.delay 10000000000
    L.writeVarIO awaitVar 1
  L.atomically $ do
    x <- L.readVar awaitVar
    when (x == 1) L.retry
