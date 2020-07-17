module Hydra.Core.Logger.Impl.HsLoggerInterpreter where

import           Hydra.Prelude

import qualified Data.Text                 as T (unpack)
import           System.Log.Logger

import qualified Hydra.Core.Language       as L
import qualified Hydra.Core.Domain         as D
import           Hydra.Core.Logger.Impl.HsLogger
import qualified Hydra.Core.Logger.Impl.HsLogger as Impl


-- TODO: Church version of flusher.
-- | Writes all stm entries into real logger.
flushStmLogger :: TVar D.Log -> Maybe Impl.HsLoggerHandle -> IO ()
flushStmLogger stmLogVar loggerHandle = do
    l <- atomically $ do
            l <- readTVar stmLogVar
            writeTVar stmLogVar []
            pure l
    mapM_ (\(D.LogEntry level msg) -> runLoggerL loggerHandle $ L.logMessage level msg) l


-- | Interpret LoggerL language.
interpretLoggerF :: HsLoggerHandle -> L.LoggerF a -> IO a
interpretLoggerF _ (L.LogMessage level msg next) = do
    logM component (dispatchLogLevel level) $ T.unpack msg
    pure $ next ()

runLoggerL :: Maybe HsLoggerHandle -> L.LoggerL () -> IO ()
runLoggerL (Just h) l = foldF (interpretLoggerF h) l
runLoggerL Nothing  _ = pure ()
