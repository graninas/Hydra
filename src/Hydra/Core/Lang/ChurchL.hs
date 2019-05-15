{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Hydra.Core.Lang.ChurchL where

import           Hydra.Prelude

import qualified Hydra.Core.ControlFlow.ChurchL  as CL
import qualified Hydra.Core.ControlFlow.Class    as L
import qualified Hydra.Core.ControlFlow.Language as L
import qualified Hydra.Core.Logger.ChurchL       as CL
import qualified Hydra.Core.Logger.Class         as L
import qualified Hydra.Core.Logger.Language      as L
import qualified Hydra.Core.Random.ChurchL       as CL
import qualified Hydra.Core.Random.Class         as L
import qualified Hydra.Core.Random.Language      as L
import qualified Hydra.Core.State.ChurchL        as CL
import qualified Hydra.Core.State.Class          as L
import qualified Hydra.Core.State.Language       as L

import           Language.Haskell.TH.MakeFunctor (makeFunctorInstance)

-- | Core effects container language.
data LangF next where
  -- -- | Eval stateful action atomically.
  EvalStateAtomically :: CL.StateL a       -> (a -> next) -> LangF next
  -- | Logger effect
  EvalLogger          :: CL.LoggerL ()     -> (() -> next) -> LangF next
  -- | Random effect
  EvalRandom          :: CL.RandomL a      -> (a  -> next) -> LangF next
  -- | ControlFlow effect
  EvalControlFlow     :: CL.ControlFlowL a -> (a  -> next) -> LangF next
  -- -- | Impure effect. Avoid using it in production code (it's not testable).
  EvalIO              :: IO a              -> (a  -> next) -> LangF next

makeFunctorInstance ''LangF

type LangL = F LangF

-- class IOL m where
--   evalIO :: IO a -> m a
--
-- instance IOL LangL where
--   evalIO io = liftF $ EvalIO io id

-- | Eval stateful action atomically.
evalStateAtomically :: CL.StateL a -> LangL a
evalStateAtomically action = liftFC $ EvalStateAtomically action id

instance L.StateIO LangL where
  newVarIO       = evalStateAtomically . L.newVar
  readVarIO      = evalStateAtomically . L.readVar
  writeVarIO var = evalStateAtomically . L.writeVar var
  retryIO        = evalStateAtomically L.retry

instance L.Atomically CL.StateL LangL where
  atomically = evalStateAtomically

evalLogger :: CL.LoggerL () -> LangL ()
evalLogger logger = liftFC $ EvalLogger logger id

instance L.Logger LangL where
  logMessage level msg = evalLogger $ L.logMessage level msg

evalRandom :: CL.RandomL a -> LangL a
evalRandom g = liftFC $ EvalRandom g id

instance L.Random LangL where
  getRandomInt = evalRandom . L.getRandomInt

evalControlFlow :: CL.ControlFlowL a -> LangL a
evalControlFlow a = liftFC $ EvalControlFlow a id

instance L.ControlFlow LangL where
  delay i = evalControlFlow $ L.delay i
