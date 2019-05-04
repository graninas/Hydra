module Hydra.Core.Lang.FTLI where

import           Hydra.Prelude

import qualified Hydra.Core.FTL     as L
import qualified Hydra.Core.RLens   as RLens
import qualified Hydra.Core.Runtime as R

-- Compiles but wrong.
-- class Monad m => LangL m where
--   atomically :: StateL m => m a -> m a
-- Doesn't work
-- instance L.LangL (ReaderT R.CoreRuntime IO) where
--   atomically stmlAction = do
--     coreRt <- ask
--     let stmAction = runReaderT stmlAction coreRt
--     res <- liftIO $ atomically stmAction
--     pure res


-- class Monad m => LangL m
-- class StateLLangL m where
--   atomically :: (StateL sm, LangL m) => sm a -> m a
-- Doesn't work
-- instance L.StateLLangL (ReaderT R.CoreRuntime IO) where
--   atomically stmlAction = do
--     coreRt <- ask
--     let stmAction = runReaderT stmlAction coreRt
--     res <- liftIO $ atomically stmAction
--     pure res


instance L.StateL (ReaderT R.CoreRuntime IO) where
  atomically stmlAction = do
    coreRt <- ask
    let stmAction = runReaderT stmlAction coreRt
    res <- liftIO $ atomically stmAction
    pure res
