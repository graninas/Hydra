module Hydra.Core.Logger.Impl.HsLogger where

import           Hydra.Prelude

import           System.IO                 (Handle, stdout)
import           System.Log.Formatter
import           System.Log.Handler        (close, setFormatter)
import           System.Log.Handler.Simple (GenericHandler, fileHandler, streamHandler)
import           System.Log.Logger

import qualified Hydra.Core.Domain         as D

-- | Opaque type covering all information needed to teardown the logger.
data HsLoggerHandle = HsLoggerHandle
  { handlers :: [GenericHandler Handle]
  }

component :: String
component = ""

-- | Bracket an IO action which denotes the whole scope where the loggers of
-- the application are needed to installed. Sets them up before running the action
-- and tears them down afterwards. Even in case of an exception.
withLogger :: D.LoggerConfig -> (HsLoggerHandle -> IO c) -> IO c
withLogger config = bracket (setupLogger config) teardownLogger

-- | Dispatch log level from the LoggerL language
-- to the relevant log level of hslogger package
dispatchLogLevel :: D.LogLevel -> Priority
dispatchLogLevel D.Debug   = DEBUG
dispatchLogLevel D.Info    = INFO
dispatchLogLevel D.Warning = WARNING
dispatchLogLevel D.Error   = ERROR

-- | Setup logger required by the application.
setupLogger :: D.LoggerConfig -> IO HsLoggerHandle
setupLogger (D.LoggerConfig format level logFileName isConsoleLog isFileLog) = do
    let logLevel     = dispatchLogLevel level
    let setFormat lh = pure $ setFormatter lh (simpleLogFormatter format)

    let fileH        = [fileHandler logFileName logLevel >>= setFormat | isFileLog   ]
    let consoleH     = [streamHandler stdout logLevel    >>= setFormat | isConsoleLog]

    handlers <- sequence $ fileH ++ consoleH

    when (length handlers > 0) $ updateGlobalLogger rootLoggerName (setLevel DEBUG . setHandlers handlers)
    pure $ HsLoggerHandle handlers

-- TODO: FIXME: these clearings don't work for console logger.
-- | Tear down the application logger; i.e. close all associated log handlers.
teardownLogger :: HsLoggerHandle -> IO ()
teardownLogger (HsLoggerHandle handlers) = do
    let x = setHandlers @(GenericHandler Handle) []
    updateGlobalLogger rootLoggerName (setLevel EMERGENCY . x)
    mapM_ close handlers
