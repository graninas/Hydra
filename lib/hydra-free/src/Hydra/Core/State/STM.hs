{-# LANGUAGE BangPatterns #-}

module Hydra.Core.State.STM where

import           Hydra.Prelude

import qualified Data.Map           as Map
import           Unsafe.Coerce      (unsafeCoerce)

import qualified Hydra.Core.Domain  as D
import qualified Hydra.Core.RLens   as RLens
import qualified Hydra.Core.Runtime as R

getVarId :: R.StateRuntime -> STM D.VarId
getVarId stateRt = do
  !v <- readTVar $ stateRt ^. RLens.varId
  writeTVar (stateRt ^. RLens.varId) $ v + 1
  pure v

newVar' :: R.StateRuntime -> a -> STM D.VarId
newVar' stateRt !a = do
    nodeState <- takeTMVar $ stateRt ^. RLens.state
    varId     <- getVarId stateRt
    tvar      <- newTVar $ unsafeCoerce a
    let !newMap = Map.insert varId (R.VarHandle tvar) nodeState
    putTMVar (stateRt ^. RLens.state) newMap
    pure varId

readVar' :: R.StateRuntime -> D.StateVar a -> STM a
readVar' stateRt (D.StateVar !varId) = do
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
