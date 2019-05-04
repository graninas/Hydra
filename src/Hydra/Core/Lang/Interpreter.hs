module Hydra.Core.Lang.Interpreter where

import           Hydra.Prelude

import qualified Hydra.Core.Language                as L
import           Hydra.Core.Logger.Impl.HsLogger    (runLoggerL)
import           Hydra.Core.Random.Interpreter      (runRandomL)
import           Hydra.Core.ControlFlow.Interpreter (runControlFlowL)
import           Hydra.Core.Random.Interpreter      (runRandomL)
import           Hydra.Core.State.Interpreter       (runStateL)
import qualified Hydra.Core.RLens                   as RLens
import qualified Hydra.Core.Runtime                 as R

-- | Interprets core lang.
interpretLangF :: R.CoreRuntime -> L.LangF a -> IO a
interpretLangF coreRt (L.EvalStateAtomically action next) = do
    let stateRt  = coreRt ^. RLens.stateRuntime
    let loggerRt = coreRt ^. RLens.loggerRuntime
    res <- atomically $ runStateL stateRt action
    R.flushStmLogger stateRt loggerRt
    pure $ next res

interpretLangF coreRt (L.EvalLogger msg next) =
    next <$> runLoggerL (coreRt ^. RLens.loggerRuntime . RLens.hsLoggerHandle) msg
interpretLangF _      (L.EvalRandom  s next)        = next <$> runRandomL s
interpretLangF coreRt (L.EvalControlFlow f    next) = next <$> runControlFlowL coreRt f
interpretLangF _      (L.EvalIO f next)             = next <$> f

-- | Runs core lang.
runLangL :: R.CoreRuntime -> L.LangL a -> IO a
runLangL coreRt = foldFree (interpretLangF coreRt)
