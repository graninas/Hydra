module Hydra.Testing.Functional.LangInterpreter where

import           Hydra.Prelude

import qualified Data.Map as Map
import           Unsafe.Coerce (unsafeCoerce)

import qualified Hydra.Core.Domain        as D
import qualified Hydra.Core.Language      as L
import qualified Hydra.Framework.Language as L

import           Hydra.Testing.Functional.TestRuntime


interpretLangF :: TestRuntime -> L.LangF a -> IO a
interpretLangF testRt (L.EvalStateAtomically action next) =
  error "not implemented."

interpretLangF testRt (L.EvalLogger loggerAct next) =
  error "not implemented."

interpretLangF testRt (L.EvalKVDB storage act next) =
  error "not implemented."

interpretLangF testRt (L.EvalSqlDB conn sqlDbMethod next) =
  error "not implemented."

interpretLangF testRt (L.GetSqlDBConnection cfg next) =
  error "not implemented."

interpretLangF _      (L.ThrowException exc _) =
  error "not implemented."

interpretLangF testRt (L.RunSafely act next) =
  error "not implemented."

interpretLangF testRt (L.CallServantAPI bUrl clientAct next) =
  error "not implemented."

runLangL :: TestRuntime -> L.LangL a -> IO a
runLangL testRt = foldFree (interpretLangF testRt)
