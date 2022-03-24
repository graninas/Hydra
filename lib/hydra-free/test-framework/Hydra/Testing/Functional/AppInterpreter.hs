module Hydra.Testing.Functional.AppInterpreter where

import           Hydra.Prelude

import           Unsafe.Coerce (unsafeCoerce)

import qualified Hydra.Core.Domain        as D
import qualified Hydra.Core.Language      as L
import qualified Hydra.Framework.Language as L
import qualified Hydra.Runtime            as R
import qualified Hydra.Interpreters       as R

import           Hydra.Testing.Functional.TestRuntime
import           Hydra.Testing.Functional.Common
import qualified Hydra.Testing.Functional.LangInterpreter as F
import qualified Hydra.Testing.Functional.RLens as RLens


interpretAppF :: TestRuntime -> L.AppF a -> IO a
interpretAppF testRt f@(L.EvalLang action next) =
  withStep "AppF.EvalLang" testRt next
    (F.runLangL testRt action)
    (id, \appRt -> R.interpretAppF appRt f)

interpretAppF testRt (L.EvalProcess action next) =
  error "AppF.EvalProcess test interpreter not implemented."

interpretAppF testRt f@(L.InitKVDB cfg dbName next) =
  withStep "AppF.InitKVDB" testRt next
    (initKVDB' testRt cfg dbName)
    (id, \appRt -> R.interpretAppF appRt f)

interpretAppF testRt (L.InitSqlDB cfg next) =
  error "AppF.InitSqlDB test interpreter not implemented."

interpretAppF testRt (L.CliF completeFunc onStep onUnknownCommand onParamsParseError handlers cliToken next) =
  error "AppF.CliF test interpreter not implemented."

runAppL :: TestRuntime -> L.AppL a -> IO a
runAppL testRt = foldFree (interpretAppF testRt)
