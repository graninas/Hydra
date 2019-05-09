module Hydra.Core.Logger.Impl.HsLoggerChurchI where

import           Hydra.Prelude

import qualified Data.Text                 as TXT (unpack)
import           System.IO                 (Handle, stdout)
import           System.Log.Formatter
import           System.Log.Handler        (close, setFormatter)
import           System.Log.Handler.Simple (GenericHandler, fileHandler, streamHandler)
import           System.Log.Logger

import qualified Hydra.Core.Domain         as D
import qualified Hydra.Core.ChurchL        as CL
import qualified Hydra.Core.Language       as L
import           Hydra.Core.Logger.Impl.HsLogger
import qualified Hydra.Core.Logger.Impl.HsLoggerInterpreter as I


runLoggerL :: Maybe HsLoggerHandle -> CL.LoggerL () -> IO ()
runLoggerL (Just h) l = foldF (I.interpretLoggerF h) l
runLoggerL Nothing  _ = pure ()
