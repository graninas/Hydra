module Hydra.Core.State.Interpreter where

import           Hydra.Prelude

import qualified Data.Map                         as Map
import           Unsafe.Coerce                    (unsafeCoerce)

import qualified Hydra.Core.Language              as L
import qualified Hydra.Core.RLens                 as RLens
import qualified Hydra.Core.Runtime               as R
import qualified Hydra.Core.Domain                as D

import           Hydra.Core.Logger.Impl.StmLogger (runStmLoggerL)

getVarId :: R.StateRuntime -> STM D.VarId
getVarId stateRt = do
  v <- readTVar $ stateRt ^. RLens.varId
  writeTVar (stateRt ^. RLens.varId) $ v + 1
  pure v

newVar' :: R.StateRuntime -> a -> STM D.VarId
newVar' stateRt a = do
    nodeState <- takeTMVar $ stateRt ^. RLens.state
    varId     <- getVarId stateRt
    tvar      <- newTVar $ unsafeCoerce a
    putTMVar (stateRt ^. RLens.state) $ Map.insert varId (R.VarHandle tvar) nodeState
    pure varId

readVar' :: R.StateRuntime -> D.StateVar a -> STM a
readVar' stateRt (D.StateVar varId) = do
    nodeState <- readTMVar $ stateRt ^. RLens.state
    case Map.lookup varId nodeState of
        Nothing                 -> error $ "Var not found: " +|| varId ||+ "."
        Just (R.VarHandle tvar) -> unsafeCoerce <$> readTVar tvar

writeVar' :: R.StateRuntime -> D.StateVar a -> a -> STM ()
writeVar' stateRt (D.StateVar varId) val = do
    nodeState <- readTMVar $ stateRt ^. RLens.state
    case Map.lookup varId nodeState of
        Nothing                 -> error $ "Var not found: " +|| varId ||+ "."
        Just (R.VarHandle tvar) -> writeTVar tvar $ unsafeCoerce val


-- | Interpret StateF as STM.
interpretStateF :: R.StateRuntime -> L.StateF a -> STM a
interpretStateF stateRt (L.NewVar  val next     )  = next . D.StateVar <$> newVar' stateRt val
interpretStateF stateRt (L.ReadVar var next     )  = next <$> readVar' stateRt var
interpretStateF stateRt (L.WriteVar var val next)  = next <$> writeVar' stateRt var val
interpretStateF _       (L.Retry _              )  = retry
interpretStateF stateRt (L.EvalStmLogger act next) = next <$> runStmLoggerL (stateRt ^. RLens.stmLog) act

-- | Runs state model as STM.
runStateL :: R.StateRuntime -> L.StateL a -> STM a
runStateL stateRt = foldFree (interpretStateF stateRt)
