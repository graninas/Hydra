module Hydra.Core.Lang.FTLI where

import           Hydra.Prelude

import qualified Hydra.Core.FTL               as L
import           Hydra.Core.Logger.FTLI       ()
import           Hydra.Core.Random.FTLI       ()
import qualified Hydra.Core.RLens             as RLens
import qualified Hydra.Core.Runtime           as R
import qualified Hydra.Core.State.Interpreter as Impl

instance L.LangL (ReaderT R.CoreRuntime IO) where
-- instance MonadIO m => L.LangL (ReaderT R.CoreRuntime m) where
  evalStateAtomically action = do
    coreRt <- ask
    let stateRt  = coreRt ^. RLens.stateRuntime
    let loggerRt = coreRt ^. RLens.loggerRuntime
    res <- liftIO $ atomically $ Impl.runStateL stateRt action
    liftIO $ R.flushStmLogger stateRt loggerRt
    pure res

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
