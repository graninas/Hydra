module Hydra.Core.Process.Interpreter where

import           Hydra.Prelude

import qualified Data.Map                  as M

import qualified Hydra.Core.Domain.Process as D
import qualified Hydra.Core.Language       as L
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

interpretProcessF :: LangRunner m' -> R.ProcessRuntime -> L.ProcessF m' a -> IO a
interpretProcessF runner processRt (L.ForkProcess action next) = do
    (pPtr, pVar) <- getNextProcessId processRt >>= D.createProcessPtr
    threadId <- forkIO $ do
        res <- runLang runner action
        atomically $ putTMVar pVar res
    addProcess processRt pPtr threadId
    pure $ next pPtr

interpretProcessF _ processRt (L.KillProcess pId next) = do
    mbThreadId <- popProcess processRt pId
    whenJust mbThreadId killThread
    pure $ next ()

interpretProcessF _ _ (L.TryGetResult pPtr next) = do
    let pVar = D.getProcessVar pPtr
    mbResult <- atomically $ tryReadTMVar pVar
    pure $ next mbResult

interpretProcessF _ _ (L.AwaitResult pPtr next) = do
    let pVar = D.getProcessVar pPtr
    result <- atomically $ takeTMVar pVar
    pure $ next result

runProcessL :: LangRunner m' -> R.ProcessRuntime -> L.ProcessL m' a -> IO a
runProcessL runner processRt = foldFree (interpretProcessF runner processRt)
