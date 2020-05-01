{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Hydra.Core.Lang.ChurchL where

import           Hydra.Prelude

import qualified Hydra.Core.ControlFlow.ChurchL  as CL
import qualified Hydra.Core.ControlFlow.Class    as C
import qualified Hydra.Core.ControlFlow.Language as L
import qualified Hydra.Core.Logger.ChurchL       as CL
import qualified Hydra.Core.Logger.Class         as C
import qualified Hydra.Core.Logger.Language      as L
import qualified Hydra.Core.Random.ChurchL       as CL
import qualified Hydra.Core.Random.Class         as C
import qualified Hydra.Core.Random.Language      as L
import qualified Hydra.Core.State.ChurchL        as CL
import qualified Hydra.Core.State.Class          as C
import qualified Hydra.Core.State.Language       as L
import qualified Hydra.Core.Lang.Class           as C

import           Language.Haskell.TH.MakeFunctor (makeFunctorInstance)

-- | Core effects container language.
data LangF next where
  -- | Eval stateful action atomically.
  EvalStateAtomically :: CL.StateL a       -> (a -> next) -> LangF next
  -- | Logger effect
  EvalLogger          :: CL.LoggerL ()     -> (() -> next) -> LangF next
  -- | Random effect
  EvalRandom          :: CL.RandomL a      -> (a  -> next) -> LangF next
  -- | ControlFlow effect
  EvalControlFlow     :: CL.ControlFlowL a -> (a  -> next) -> LangF next
  -- | Impure effect. Avoid using it in production code (it's not testable).
  EvalIO              :: IO a              -> (a  -> next) -> LangF next

  -- TODO: KVDB

makeFunctorInstance ''LangF

type LangL = F LangF

instance C.IOL LangL where
  evalIO io = liftFC $ EvalIO io id

evalStateAtomically' :: CL.StateL a -> LangL a
evalStateAtomically' action = liftFC $ EvalStateAtomically action id

evalLogger' :: CL.LoggerL () -> LangL ()
evalLogger' logger = liftFC $ EvalLogger logger id

evalRandom' :: CL.RandomL a -> LangL a
evalRandom' g = liftFC $ EvalRandom g id

evalControlFlow' :: CL.ControlFlowL a -> LangL a
evalControlFlow' a = liftFC $ EvalControlFlow a id

instance C.Lang CL.LoggerL CL.RandomL CL.ControlFlowL CL.StateL LangL where
  evalStateAtomically = evalStateAtomically'
  evalLogger          = evalLogger'
  evalRandom          = evalRandom'
  evalControlFlow     = evalControlFlow'

instance C.StateIO LangL where
  newVarIO       = evalStateAtomically' . C.newVar
  readVarIO      = evalStateAtomically' . C.readVar
  writeVarIO var = evalStateAtomically' . C.writeVar var
  retryIO        = evalStateAtomically' C.retry

instance C.Atomically CL.StateL LangL where
  atomically = evalStateAtomically'

instance C.Logger LangL where
  logMessage level msg = evalLogger' $ C.logMessage level msg

instance C.Random LangL where
  getRandomInt = evalRandom' . C.getRandomInt

instance C.ControlFlow LangL where
  delay i = evalControlFlow' $ C.delay i
