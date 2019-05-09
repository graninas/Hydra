module Hydra.Core.Lang.ChurchI where

import           Hydra.Prelude

import           Hydra.Core.ControlFlow.ChurchI (runControlFlowL)
import qualified Hydra.Core.Lang.ChurchL        as CL
import qualified Hydra.Core.Lang.Interpreter    as I
import qualified Hydra.Core.Lang.Language       as L
import qualified Hydra.Core.RLens               as RLens
import qualified Hydra.Core.Runtime             as R

-- | Interprets core lang.
interpretLangF :: R.CoreRuntime -> L.LangF a -> IO a
-- interpretLangF coreRt (L.EvalStateAtomically action next) = do
    -- let stateRt  = coreRt ^. RLens.stateRuntime
    -- let loggerRt = coreRt ^. RLens.loggerRuntime
    -- res <- atomically $ runStateL stateRt action
    -- R.flushStmLogger stateRt loggerRt
    -- pure $ next res
interpretLangF coreRt (L.EvalControlFlow f    next) = next <$> runControlFlowL coreRt f
-- interpretLangF coreRt (L.EvalLogger msg next) =
--     next <$> runLoggerL (coreRt ^. RLens.loggerRuntime . RLens.hsLoggerHandle) msg
-- interpretLangF _      (L.EvalRandom  s next)        = next <$> runRandomL s
-- interpretLangF _      (L.EvalIO f next)             = next <$> f
interpretLangF _ _ = error "Not implemented."

-- | Runs core lang.
runLangL :: R.CoreRuntime -> CL.LangL a -> IO a
runLangL coreRt = foldF (interpretLangF coreRt)
