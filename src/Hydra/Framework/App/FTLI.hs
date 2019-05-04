module Hydra.Core.Lang.FTLI where

import           Hydra.Prelude

import qualified Hydra.Core.FTL                     as L
import qualified Hydra.Core.RLens                   as RLens
import qualified Hydra.Core.Runtime                 as R

instance L.IOL (ReaderT R.CoreRuntime IO) where
  evalIO io = liftIO io

instance L.LangL (ReaderT R.CoreRuntime IO) where
  evalStateAtomically action = do
      coreRt <- ask
      let loggerRt = coreRt ^. RLens.loggerRuntime
      res <- liftIO $ atomically action
      liftiO $ R.flushStmLogger stateRt loggerRt
      pure res
  evalLogger = error "Logger not implemented"
  evalRandom = error "Logger not implemented"
  evalControlFlow = error "Logger not implemented"
