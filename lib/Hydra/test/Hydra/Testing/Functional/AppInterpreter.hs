module Hydra.Testing.Functional.AppInterpreter where

import           Hydra.Prelude

import qualified Data.Map as Map

import qualified Hydra.Core.Domain        as D
import qualified Hydra.Core.Language      as L
import qualified Hydra.Framework.Language as L

import           Hydra.Testing.Functional.TestRuntime


interpretAppF :: TestRuntime -> L.AppF a -> IO a
interpretAppF testRt@(TestRuntime {evalLangMocks}) (L.EvalLang action next) = do
  mbMock <- popNextMock evalLangMocks
  case mbMock of
    Just (Mock ghcAny)      -> pure $ next $ unsafeCoerce ghcAny
    Just RunTestInterpreter -> next <$> runLangL testRt action
    Just RunRealInterpreter -> error "Real interpreter running not implemented."
    Nothing                 -> error "Mock not found."

interpretAppF testRt (L.EvalProcess action next) =
  error "EvalProcess test interpreter not implemented."

interpretAppF testRt (L.InitKVDB cfg dbName next) =
  error "InitKVDB test interpreter not implemented."

interpretAppF testRt (L.InitSqlDB cfg next) =
  error "InitSqlDB test interpreter not implemented."

interpretAppF testRt (L.CliF completeFunc onStep onUnknownCommand handlers cliToken next) =
  error "CliF test interpreter not implemented."

runAppL :: TestRuntime -> L.AppL a -> IO a
runAppL testRt = foldFree (interpretAppF testRt)
