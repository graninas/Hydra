module Hydra.Framework.App.Interpreter where

import           Hydra.Prelude

import qualified Hydra.Core.Interpreter   as I
import qualified Hydra.Core.Language      as L
import qualified Hydra.Core.RLens         as RLens
import qualified Hydra.Core.Runtime       as R

import qualified Hydra.Framework.Language as L

import qualified Hydra.Framework.RLens    as RLens
import qualified Hydra.Framework.Runtime  as R

langRunner :: R.CoreRuntime -> I.LangRunner L.LangL
langRunner coreRt = I.LangRunner (I.runLangL coreRt)

interpretAppF :: R.AppRuntime -> L.AppF a -> IO a
interpretAppF appRt (L.EvalLang action next) = do
  let coreRt = appRt ^. RLens.coreRuntime
  res <- I.runLangL coreRt action
  pure $ next res

interpretAppF appRt (L.EvalProcess action next) = do
  let coreRt = appRt ^. RLens.coreRuntime
  res <- I.runProcessL (langRunner coreRt) (coreRt ^. RLens.processRuntime) action
  pure $ next res

runAppL :: R.AppRuntime -> L.AppL a -> IO a
runAppL appRt = foldF (interpretAppF appRt)
