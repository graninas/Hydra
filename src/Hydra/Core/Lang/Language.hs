{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module Hydra.Core.Lang.Language where

import           Hydra.Prelude

import           Hydra.Core.ControlFlow.Language as L
import           Hydra.Core.Logger.Language      as L
import           Hydra.Core.Random.Language      as L
import           Hydra.Core.State.Language       as L

import           Language.Haskell.TH.MakeFunctor (makeFunctorInstance)

-- | Core effects container language.
data LangF next where
  -- | Eval stateful action atomically.
  EvalStateAtomically :: L.StateL a -> (a -> next) -> LangF next
  -- | Logger effect
  EvalLogger      :: L.LoggerL ()     -> (() -> next) -> LangF next
  -- | Random effect
  EvalRandom      :: L.RandomL a     -> (a  -> next) -> LangF next
  -- | ControlFlow effect
  EvalControlFlow :: L.ControlFlowL a -> (a  -> next) -> LangF next
  -- | Impure effect. Avoid using it in production code (it's not testable).
  EvalIO          :: IO a           -> (a  -> next) -> LangF next

makeFunctorInstance ''LangF

type LangL = Free LangF

class IOL m where
  evalIO :: IO a -> m a

instance IOL LangL where
  evalIO io = liftF $ EvalIO io id

-- | Eval stateful action atomically.
evalStateAtomically :: L.StateL a -> LangL a
evalStateAtomically action = liftF $ EvalStateAtomically action id

instance L.StateIO LangL where
    atomically     = evalStateAtomically
    newVarIO       = evalStateAtomically . L.newVar
    readVarIO      = evalStateAtomically . L.readVar
    writeVarIO var = evalStateAtomically . L.writeVar var

evalLogger :: L.LoggerL () -> LangL ()
evalLogger logger = liftF $ EvalLogger logger id

instance Logger LangL where
  logMessage level msg = evalLogger $ logMessage level msg

evalRandom :: L.RandomL a -> LangL a
evalRandom g = liftF $ EvalRandom g id

instance L.Random LangL where
  getRandomInt = evalRandom . getRandomInt

evalControlFlow :: L.ControlFlowL a -> LangL a
evalControlFlow a = liftF $ EvalControlFlow a id

instance L.ControlFlow LangL where
  delay i = evalControlFlow $ L.delay i
