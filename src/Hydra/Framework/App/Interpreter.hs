module Hydra.Framework.App.Interpreter where

import           Hydra.Prelude

import qualified Hydra.Core.Domain        as D
import qualified Hydra.Core.Interpreters  as Impl
import qualified Hydra.Core.Language      as L
import qualified Hydra.Core.RLens         as RLens
import qualified Hydra.Core.Runtime       as R
import qualified Hydra.Framework.Language as L
import qualified Hydra.Framework.RLens    as RLens
import qualified Hydra.Framework.Runtime  as R

langRunner :: R.CoreRuntime -> Impl.LangRunner L.LangL
langRunner coreRt = Impl.LangRunner (Impl.runLangL coreRt)

interpretAppF :: R.CoreRuntime -> L.AppF a -> IO a
interpretAppF coreRt (L.EvalLang action next) = do
    res <- Impl.runLangL coreRt action
    pure $ next res

interpretAppF coreRt (L.EvalProcess action next) = do
    res <- Impl.runProcessL (langRunner coreRt) (coreRt ^. RLens.processRuntime) action
    pure $ next res

runAppL :: R.CoreRuntime -> L.AppL a -> IO a
runAppL coreRt = foldFree (interpretAppF coreRt)
