module Hydra.Core.ControlFlow.FTL where

import           Hydra.Prelude

class Monad m => ControlFlowL m where
  delay :: Int -> m ()

instance ControlFlowL IO where
  delay = threadDelay
  {-# INLINE delay #-}

instance ControlFlowL (ReaderT e IO) where
  delay = lift . threadDelay
  {-# INLINE delay #-}
