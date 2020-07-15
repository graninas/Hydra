module Hydra.Core.Domain.Process
    ( ProcessId
    , ProcessPtr
    , ProcessVar
    , createProcessPtr
    , getProcessId
    , getProcessVar
    ) where

import Hydra.Prelude

type ProcessVar a = TMVar a
type ProcessId = Int
data ProcessPtr a = ProcessPtr ProcessId (ProcessVar a)

createProcessPtr :: ProcessId -> IO (ProcessPtr a, ProcessVar a)
createProcessPtr pId = do
    pVar <- newEmptyTMVarIO
    pure (ProcessPtr pId pVar, pVar)

getProcessId :: ProcessPtr a -> ProcessId
getProcessId (ProcessPtr pId _) = pId

getProcessVar :: ProcessPtr a -> ProcessVar a
getProcessVar (ProcessPtr _ pVar) = pVar
