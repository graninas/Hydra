{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Hydra.Core.Lang.Language where

import           Hydra.Prelude

import qualified Hydra.Core.ControlFlow.Language as L
import qualified Hydra.Core.ControlFlow.Class    as C
import qualified Hydra.Core.Logger.Language      as L
import qualified Hydra.Core.Logger.Class         as C
import qualified Hydra.Core.Random.Language      as L
import qualified Hydra.Core.Random.Class         as C
import qualified Hydra.Core.State.Language       as L
import qualified Hydra.Core.State.Class          as C
import qualified Hydra.Core.Lang.Class           as C

-- | Core effects container language.
data LangF next where
  -- | Eval stateful action atomically.
  EvalStateAtomically :: L.StateL a       -> (a -> next) -> LangF next
  -- | Logger effect
  EvalLogger          :: L.LoggerL ()     -> (() -> next) -> LangF next
  -- | Random effect
  EvalRandom          :: L.RandomL a      -> (a  -> next) -> LangF next
  -- | ControlFlow effect
  EvalControlFlow     :: L.ControlFlowL a -> (a  -> next) -> LangF next
  -- | Impure effect. Avoid using it in production code (it's not testable).
  EvalIO              :: IO a              -> (a  -> next) -> LangF next

  -- TODO: KVDB

instance Functor LangF where
  fmap f (EvalStateAtomically act next) = EvalStateAtomically act (f . next)
  fmap f (EvalLogger          act next) = EvalLogger          act (f . next)
  fmap f (EvalRandom          act next) = EvalRandom          act (f . next)
  fmap f (EvalControlFlow     act next) = EvalControlFlow     act (f . next)
  fmap f (EvalIO              act next) = EvalIO              act (f . next)

type LangL = F LangF

instance C.IOL LangL where
  evalIO io = liftFC $ EvalIO io id

evalStateAtomically' :: L.StateL a -> LangL a
evalStateAtomically' action = liftFC $ EvalStateAtomically action id

evalLogger' :: L.LoggerL () -> LangL ()
evalLogger' logger = liftFC $ EvalLogger logger id

evalRandom' :: L.RandomL a -> LangL a
evalRandom' g = liftFC $ EvalRandom g id

evalControlFlow' :: L.ControlFlowL a -> LangL a
evalControlFlow' a = liftFC $ EvalControlFlow a id

instance C.Lang L.LoggerL L.RandomL L.ControlFlowL L.StateL LangL where
  evalStateAtomically = evalStateAtomically'
  evalLogger          = evalLogger'
  evalRandom          = evalRandom'
  evalControlFlow     = evalControlFlow'

instance C.StateIO LangL where
  newVarIO       = evalStateAtomically' . C.newVar
  readVarIO      = evalStateAtomically' . C.readVar
  writeVarIO var = evalStateAtomically' . C.writeVar var
  retryIO        = evalStateAtomically' C.retry

instance C.Atomically L.StateL LangL where
  atomically = evalStateAtomically'

instance C.Logger LangL where
  logMessage level msg = evalLogger' $ C.logMessage level msg

instance C.Random LangL where
  getRandomInt = evalRandom' . C.getRandomInt

instance C.ControlFlow LangL where
  delay i = evalControlFlow' $ C.delay i
