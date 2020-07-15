{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hydra.Core.Process.FTLI where


--   forkProcess action = do
--     coreRt <- ask
--     let processRt = coreRt ^. RLens.processRuntime
--     (pPtr, pVar) <- liftIO (Impl.getNextProcessId processRt >>= D.createProcessPtr)
--     threadId <- liftIO $ forkIO $ do
--         res <- evaluate action coreRt
--         atomically $ putTMVar pVar res
--     liftIO $ Impl.addProcess processRt pPtr threadId
--     pure pPtr

  -- killProcess  :: D.ProcessPtr a -> m ()
  -- tryGetResult :: D.ProcessPtr a -> m (Maybe a)
  -- awaitResult  :: D.ProcessPtr a -> m a
--
-- interpretProcessF runner processRt (L.ForkProcess action next) = do
--
--
-- interpretProcessF _ processRt (L.KillProcess pId next) = do
--     mbThreadId <- popProcess processRt pId
--     whenJust mbThreadId killThread
--     pure $ next ()
--
-- interpretProcessF _ _ (L.TryGetResult pPtr next) = do
--     let pVar = D.getProcessVar pPtr
--     mbResult <- atomically $ tryReadTMVar pVar
--     pure $ next mbResult
--
-- interpretProcessF _ _ (L.AwaitResult pPtr next) = do
--     let pVar = D.getProcessVar pPtr
--     result <- atomically $ takeTMVar pVar
--     pure $ next result
