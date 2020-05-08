{-# LANGUAGE ExplicitForAll #-}

module Hydra.Core.Process.Impl where

import           Hydra.Prelude

import qualified Data.Map                  as M

import qualified Hydra.Core.Domain.Process as D
import qualified Hydra.Core.RLens          as RLens
import qualified Hydra.Core.Runtime        as R

newtype LangRunner m' = LangRunner (forall a. m' a -> IO a)

runLang :: LangRunner m' -> m' a -> IO a
runLang (LangRunner runner) action = runner action

getNextProcessId :: R.ProcessRuntime -> IO Int
getNextProcessId processRt = atomicModifyIORef' (processRt ^. RLens.idCounter) (\a -> (a + 1, a + 1))

addProcess :: R.ProcessRuntime -> D.ProcessPtr a -> ThreadId -> IO ()
addProcess procRt pPtr threadId = do
    let pId = D.getProcessId pPtr
    ps <- readTVarIO $ procRt ^. RLens.processes
    let newPs = M.insert pId threadId ps
    atomically $ writeTVar (procRt ^. RLens.processes) newPs

popProcess :: R.ProcessRuntime -> D.ProcessPtr a -> IO (Maybe ThreadId)
popProcess procRt pPtr = do
    let pId = D.getProcessId pPtr
    ps <- readTVarIO $ procRt ^. RLens.processes
    let mbThreadId = M.lookup pId ps
    let newPs = M.delete pId ps
    atomically $ writeTVar (procRt ^. RLens.processes) newPs
    pure mbThreadId
