module Hydra.Framework.App.Interpreter where

import           Hydra.Prelude

import qualified Hydra.Core.Interpreters as Impl
import qualified Hydra.Core.Language as L
import qualified Hydra.Core.Domain   as D
import qualified Hydra.Core.Runtime   as R
import qualified Hydra.Framework.Language as L
import qualified Hydra.Framework.RLens     as RLens
import qualified Hydra.Framework.Runtime   as R

langRunner :: R.CoreRuntime -> Impl.LangRunner L.LangL
langRunner coreRt = Impl.LangRunner (Impl.runLangL coreRt)

interpretAppF :: R.AppRuntime -> L.AppF a -> IO a
interpretAppF appRt (L.EvalLang action next) = do
    res <- Impl.runLangL (appRt ^. RLens.coreRuntime) action
    pure $ next res

interpretAppF appRt (L.EvalProcess action next) = do
    let coreRt = appRt ^. RLens.coreRuntime
    res <- Impl.runProcessL (langRunner coreRt) (appRt ^. RLens.processRuntime) action
    pure $ next res

runAppL :: R.AppRuntime -> L.AppL a -> IO a
runAppL appRt = foldFree (interpretAppF appRt)
