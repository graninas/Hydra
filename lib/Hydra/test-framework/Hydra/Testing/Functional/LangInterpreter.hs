module Hydra.Testing.Functional.LangInterpreter where

import           Hydra.Prelude

import           Control.Exception (throwIO)
import           Unsafe.Coerce (unsafeCoerce)

import qualified Hydra.Core.Domain        as D
import qualified Hydra.Core.Language      as L
import qualified Hydra.Framework.Language as L
import qualified Hydra.Runtime            as R
import qualified Hydra.Interpreters       as R

import           Hydra.Testing.Functional.TestRuntime
import           Hydra.Testing.Functional.Common
import qualified Hydra.Testing.Functional.RLens as RLens
import qualified Hydra.Framework.RLens          as RLens



interpretLangF :: TestRuntime -> L.LangF a -> IO a
interpretLangF testRt (L.EvalStateAtomically action next) =
  error "LangF.EvalStateAtomically not implemented."

interpretLangF testRt (L.EvalLogger loggerAct next) =
  error "LangF.EvalLogger not implemented."

interpretLangF testRt (L.EvalRandom  s next) =
  error "LangF.EvalRandom not implemented."

interpretLangF testRt (L.EvalControlFlow f next) =
  error "LangF.EvalControlFlow not implemented."

interpretLangF testRt (L.EvalIO f next) =
  error "LangF.EvalIO not implemented."

interpretLangF testRt (L.EvalKVDB storage act next) =
  error "LangF.EvalKVDB not implemented."

interpretLangF testRt (L.EvalSqlDB conn sqlDbMethod next) =
  error "LangF.EvalSqlDB not implemented."

interpretLangF testRt (L.GetSqlDBConnection cfg next) =
  error "LangF.GetSqlDBConnection not implemented."

interpretLangF testRt f@(L.ThrowException exc next) =
  withStep "LangF.ThrowException" testRt next
    (throwIO exc)
    (R._coreRuntime, \coreRt -> R.interpretLangF coreRt f)

interpretLangF testRt (L.RunSafely act next) =
  error "LangF.RunSafely not implemented."

interpretLangF testRt (L.CallServantAPI bUrl clientAct next) =
  error "LangF.CallServantAPI not implemented."

runLangL :: TestRuntime -> L.LangL a -> IO a
runLangL testRt = foldFree (interpretLangF testRt)
