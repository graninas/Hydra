module Hydra.Core.Logger.Impl.HsLoggerInterpreter where

import           Hydra.Prelude

import qualified Data.Text                 as TXT (unpack)
import           System.Log.Logger

import qualified Hydra.Core.Language       as L
import           Hydra.Core.Logger.Impl.HsLogger

-- | Interpret LoggerL language.
interpretLoggerF :: HsLoggerHandle -> L.LoggerF a -> IO a
interpretLoggerF _ (L.LogMessage level msg next) = do
    logM component (dispatchLogLevel level) $ TXT.unpack msg
    pure $ next ()

runLoggerL :: Maybe HsLoggerHandle -> L.LoggerL () -> IO ()
runLoggerL (Just h) l = foldFree (interpretLoggerF h) l
runLoggerL Nothing  _ = pure ()
