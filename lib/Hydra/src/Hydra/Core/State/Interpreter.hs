{-# LANGUAGE BangPatterns #-}

module Hydra.Core.State.Interpreter where

import           Hydra.Prelude


import qualified Hydra.Core.Domain                as D
import qualified Hydra.Core.Language              as L
import qualified Hydra.Core.RLens                 as RLens
import qualified Hydra.Core.Runtime               as R
import           Hydra.Core.State.STM

import           Hydra.Core.Logger.Impl.StmLogger (runStmLoggerL)

-- | Interpret StateF as STM.
interpretStateF :: R.StateRuntime -> L.StateF a -> STM a
interpretStateF stateRt (L.NewVar  !val next     )  = next . D.StateVar <$> newVar' stateRt val
interpretStateF stateRt (L.ReadVar var next     )  = next <$> readVar' stateRt var
interpretStateF stateRt (L.WriteVar var !val next)  = next <$> writeVar' stateRt var val
interpretStateF _       (L.Retry _              )  = retry
interpretStateF stateRt (L.EvalStmLogger act next) = next <$> runStmLoggerL (stateRt ^. RLens.stmLog) act

-- | Runs state model as STM.
runStateL :: R.StateRuntime -> L.StateL a -> STM a
runStateL stateRt = foldFree (interpretStateF stateRt)
