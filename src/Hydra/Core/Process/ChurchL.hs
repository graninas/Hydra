{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module Hydra.Core.Process.ChurchL where

import           Hydra.Prelude

import qualified Hydra.Core.Domain           as D
import qualified Hydra.Core.Process.Language as L

type ProcessL m = F (L.ProcessF m)

-- | Fork a process.
forkProcess' :: m a -> ProcessL m (D.ProcessPtr a)
forkProcess' action = liftFC $ L.ForkProcess action id

-- | Hardly kill a process.
killProcess' :: D.ProcessPtr a -> ProcessL m ()
killProcess' processPtr = liftFC $ L.KillProcess processPtr id

-- | Try get result from a process (non-blocking).
tryGetResult' :: D.ProcessPtr a -> ProcessL m (Maybe a)
tryGetResult' handle = liftFC $ L.TryGetResult handle id

-- | Await for result from a process (blocking).
awaitResult' :: D.ProcessPtr a -> ProcessL m a
awaitResult' handle = liftFC $ L.AwaitResult handle id
