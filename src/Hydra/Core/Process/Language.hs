{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module Hydra.Core.Process.Language where

import           Hydra.Prelude

import qualified Hydra.Core.Domain               as D

import           Language.Haskell.TH.MakeFunctor (makeFunctorInstance)

-- | Language for Process.
data ProcessF m' next where
    -- | Fork a process for node.
    ForkProcess :: m' a -> (D.ProcessPtr a -> next) -> ProcessF m' next
    -- | Hardly kill the process.
    KillProcess :: D.ProcessPtr a -> (() -> next) -> ProcessF m' next
    -- | Try get result (non-blocking).
    TryGetResult :: D.ProcessPtr a -> (Maybe a -> next) -> ProcessF m' next
    -- | Await for result (blocking).
    AwaitResult :: D.ProcessPtr a -> (a -> next) -> ProcessF m' next

instance Functor (ProcessF m') where
    fmap f (ForkProcess action next) = ForkProcess action (f . next)
    fmap f (KillProcess pPtr next)   = KillProcess pPtr (f . next)
    fmap f (TryGetResult pPtr next)  = TryGetResult pPtr (f . next)
    fmap f (AwaitResult pPtr next)   = AwaitResult pPtr (f . next)

type ProcessL m' = Free (ProcessF m')

-- | Fork a process.
forkProcess' :: m' a -> ProcessL m' (D.ProcessPtr a)
forkProcess' action = liftF $ ForkProcess action id

-- | Hardly kill a process.
killProcess' :: D.ProcessPtr a -> ProcessL m' ()
killProcess' processPtr = liftF $ KillProcess processPtr id

-- | Try get result from a process (non-blocking).
tryGetResult' :: D.ProcessPtr a -> ProcessL m' (Maybe a)
tryGetResult' handle = liftF $ TryGetResult handle id

-- | Await for result from a process (blocking).
awaitResult' :: D.ProcessPtr a -> ProcessL m' a
awaitResult' handle = liftF $ AwaitResult handle id
