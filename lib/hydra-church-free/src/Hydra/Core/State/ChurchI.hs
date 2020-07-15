{-# LANGUAGE BangPatterns #-}

module Hydra.Core.State.ChurchI where

import           Hydra.Prelude

import qualified Hydra.Core.Domain                      as D
import qualified Hydra.Core.RLens                       as RLens
import qualified Hydra.Core.Runtime                     as R
import qualified Hydra.Core.State.ChurchL               as CL
import           Hydra.Core.State.STM

import           Hydra.Core.Logger.Impl.StmLoggerChurch (runStmLoggerL)

-- | Interpret StateF as STM.
interpretStateF :: R.StateRuntime -> CL.StateF a -> STM a
interpretStateF stateRt (CL.NewVar  !val next     )  = next . D.StateVar <$> newVar' stateRt val
interpretStateF stateRt (CL.ReadVar var next     )   = next <$> readVar' stateRt var
interpretStateF stateRt (CL.WriteVar var !val next)  = next <$> writeVar' stateRt var val
interpretStateF _       (CL.Retry _              )   = retry
interpretStateF stateRt (CL.EvalStmLogger act next)  = next <$> runStmLoggerL (stateRt ^. RLens.stmLog) act

-- | Runs state model as STM.
runStateL :: R.StateRuntime -> CL.StateL a -> STM a
runStateL stateRt = foldF (interpretStateF stateRt)
