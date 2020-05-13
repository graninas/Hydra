module Hydra.Core.Lang.ChurchI where

import           Hydra.Prelude

import           Hydra.Core.ControlFlow.ChurchI         (runControlFlowL)
import qualified Hydra.Core.Lang.ChurchL                as CL
import           Hydra.Core.Logger.Impl.HsLoggerChurchI (runLoggerL)
import           Hydra.Core.Random.ChurchI              (runRandomL)
import qualified Hydra.Core.RLens                       as RLens
import qualified Hydra.Core.Runtime                     as R
import           Hydra.Core.State.ChurchI               (runStateL)

-- | Interprets core lang.
interpretLangF :: R.CoreRuntime -> CL.LangF a -> IO a
interpretLangF coreRt (CL.EvalStateAtomically action next) = do
    let stateRt  = coreRt ^. RLens.stateRuntime
    let loggerRt = coreRt ^. RLens.loggerRuntime
    res <- atomically $ runStateL stateRt action
    R.flushStmLogger stateRt loggerRt
    pure $ next res
interpretLangF coreRt (CL.EvalControlFlow f    next) = next <$> runControlFlowL coreRt f
interpretLangF coreRt (CL.EvalLogger msg next) =
    next <$> runLoggerL (coreRt ^. RLens.loggerRuntime . RLens.hsLoggerHandle) msg
interpretLangF _      (CL.EvalRandom  s next)        = next <$> runRandomL s
interpretLangF _      (CL.EvalIO f next)             = next <$> f

-- | Runs core lang.
runLangL :: R.CoreRuntime -> CL.LangL a -> IO a
runLangL coreRt = foldF (interpretLangF coreRt)
