module Hydra.Framework.App.ChurchI where

import           Hydra.Prelude

import qualified Hydra.Core.ChurchI       as CI
import qualified Hydra.Core.ChurchL       as CL
import qualified Hydra.Core.RLens         as RLens
import qualified Hydra.Core.Runtime       as R

import qualified Hydra.Framework.ChurchL  as CL
import qualified Hydra.Framework.Language as L

import qualified Hydra.Framework.RLens    as RLens
import qualified Hydra.Framework.Runtime  as R

langRunner :: R.CoreRuntime -> CI.LangRunner CL.LangL
langRunner coreRt = CI.LangRunner (CI.runLangL coreRt)

interpretAppF :: R.AppRuntime -> CL.AppF a -> IO a
interpretAppF appRt (CL.EvalLang action next) = do
  let coreRt = appRt ^. RLens.coreRuntime
  res <- CI.runLangL coreRt action
  pure $ next res

interpretAppF appRt (CL.EvalProcess action next) = do
  let coreRt = appRt ^. RLens.coreRuntime
  res <- CI.runProcessL (langRunner coreRt) (coreRt ^. RLens.processRuntime) action
  pure $ next res

runAppL :: R.AppRuntime -> CL.AppL a -> IO a
runAppL appRt = foldF (interpretAppF appRt)
