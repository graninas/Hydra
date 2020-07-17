{-# LANGUAGE ExplicitForAll #-}

module Hydra.Core.Process.Interpreter where

import           Hydra.Prelude

import qualified Hydra.Core.Language       as L
import qualified Hydra.Core.Domain.Process as D
import           Hydra.Core.Process.Impl
import qualified Hydra.Core.Runtime        as R

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
runProcessL runner processRt = foldF (interpretProcessF runner processRt)
