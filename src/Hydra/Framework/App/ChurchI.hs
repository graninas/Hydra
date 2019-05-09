module Hydra.Framework.App.ChurchI where

import           Hydra.Prelude

import qualified Hydra.Core.ChurchI       as CI
import qualified Hydra.Core.ChurchL       as CL
import qualified Hydra.Core.Domain        as D
import qualified Hydra.Core.Language      as L
import qualified Hydra.Core.RLens         as RLens
import qualified Hydra.Core.Runtime       as R

import qualified Hydra.Framework.ChurchL  as CL
import qualified Hydra.Framework.Language as L

import qualified Hydra.Framework.RLens    as RLens
import qualified Hydra.Framework.Runtime  as R

-- langRunner :: R.CoreRuntime -> Impl.LangRunner CL.LangL
-- langRunner coreRt = Impl.LangRunner (Impl.runLangL coreRt)

interpretAppF :: R.CoreRuntime -> L.AppF a -> IO a
interpretAppF coreRt (L.EvalLang action next) = do
    res <- CI.runLangL coreRt action
    pure $ next res

-- interpretAppF coreRt (L.EvalProcess action next) = do
--     res <- Impl.runProcessL (langRunner coreRt) (coreRt ^. RLens.processRuntime) action
--     pure $ next res

runAppL :: R.CoreRuntime -> CL.AppL a -> IO a
runAppL coreRt = foldF (interpretAppF coreRt)
