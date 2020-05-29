module Hydra.Testing.Functional.AppInterpreter where

import           Hydra.Prelude

import qualified Data.Map as Map
import           Unsafe.Coerce (unsafeCoerce)

import qualified Hydra.Core.Domain        as D
import qualified Hydra.Core.Language      as L
import qualified Hydra.Framework.Language as L
import qualified Hydra.Runtime            as R
import qualified Hydra.Interpreters       as R

import           Hydra.Testing.Functional.TestRuntime
import           Hydra.Testing.Functional.LangInterpreter
import qualified Hydra.Testing.Functional.RLens as RLens

withStep :: TestRuntime -> b -> IO a
withStep testRt act = do
  mbMock <- popNextStep testRt
  case mbMock of
    Just (Mock ghcAny)      -> pure $ unsafeCoerce ghcAny
    Just RunTestInterpreter -> act
    Just RunRealInterpreter -> error "Real interpreter running not implemented."
    Nothing                 -> error "Mock not found."


interpretAppF :: TestRuntime -> L.AppF a -> IO a
interpretAppF testRt (L.EvalLang action next) =
  next <$> withStep testRt (
    runLangL testRt action)


interpretAppF testRt (L.EvalProcess action next) =
  error "EvalProcess test interpreter not implemented."

interpretAppF testRt (L.InitKVDB cfg dbName next) = do
  error "InitKVDB test interpreter not implemented."

interpretAppF testRt (L.InitSqlDB cfg next) =
  error "InitSqlDB test interpreter not implemented."

interpretAppF testRt (L.CliF completeFunc onStep onUnknownCommand handlers cliToken next) =
  error "CliF test interpreter not implemented."

runAppL :: TestRuntime -> L.AppL a -> IO a
runAppL testRt = foldFree (interpretAppF testRt)
