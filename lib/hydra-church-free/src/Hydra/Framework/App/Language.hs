{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Hydra.Framework.App.Language where

import           Hydra.Prelude

import qualified Hydra.Core.Language             as L
import qualified Hydra.Core.Class                as C
import qualified Hydra.Core.Domain               as D


-- | Core effects container language.
data AppF next where
  -- | Eval process.
  EvalProcess :: L.ProcessL L.LangL a -> (a -> next) -> AppF next
  -- | Eval lang.
  EvalLang    :: L.LangL a  -> (a -> next) -> AppF next

instance Functor AppF where
  fmap f (EvalProcess    act next) = EvalProcess act (f . next)
  fmap f (EvalLang       act next) = EvalLang    act (f . next)


type AppL = F AppF

-- | Eval lang.
evalLang' :: L.LangL a -> AppL a
evalLang' action = liftFC $ EvalLang action id

-- | Eval lang.
scenario :: L.LangL a -> AppL a
scenario = evalLang'

evalProcess' :: L.ProcessL L.LangL a -> AppL a
evalProcess' action = liftFC $ EvalProcess action id

instance C.Process L.LangL AppL where
  forkProcess  = evalProcess' . L.forkProcess'
  killProcess  = evalProcess' . L.killProcess'
  tryGetResult = evalProcess' . L.tryGetResult'
  awaitResult  = evalProcess' . L.awaitResult'

-- | Fork a process and keep the Process Ptr.
fork :: L.LangL a -> AppL (D.ProcessPtr a)
fork = evalProcess' . L.forkProcess'

-- | Fork a process and forget.
process :: L.LangL a -> AppL ()
process action = void $ fork action

instance C.IOL AppL where
  evalIO = evalLang' . C.evalIO

instance L.StateIO AppL where
  newVarIO       = evalLang' . L.newVarIO
  readVarIO      = evalLang' . L.readVarIO
  writeVarIO var = evalLang' . L.writeVarIO var
  retryIO        = evalLang' L.retryIO

instance L.Atomically L.StateL AppL where
  atomically = evalLang' . L.atomically

instance L.Logger AppL where
  logMessage level msg = evalLang' $ L.logMessage level msg

instance L.Random AppL where
  getRandomInt = evalLang' . L.getRandomInt

instance L.ControlFlow AppL where
  delay = evalLang' . L.delay
