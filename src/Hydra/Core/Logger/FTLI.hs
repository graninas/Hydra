module Hydra.Core.Logger.FTLI where

import           Hydra.Prelude

import qualified Hydra.Core.FTL                  as L
import qualified Hydra.Core.RLens                as RLens
import qualified Hydra.Core.Runtime              as R

import qualified Data.Text                       as TXT (unpack)
import qualified Hydra.Core.Logger.Impl.HsLogger as Hs
import qualified System.Log.Logger               as Hs

-- TODO: hslogger specific is here!
instance MonadIO m => L.LoggerL (ReaderT R.CoreRuntime m) where
  logMessage lvl msg = do
    coreRt <- ask
    let mbHsRt = coreRt ^. RLens.loggerRuntime ^. RLens.hsLoggerHandle
    when (isJust mbHsRt) $
      liftIO $ Hs.logM Hs.component (Hs.dispatchLogLevel lvl) $ TXT.unpack msg
