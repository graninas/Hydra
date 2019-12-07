{-# LANGUAGE DeriveAnyClass #-}

module Hydra.Core.Domain.Logger where

import           Hydra.Prelude

-- | Logging level.
data LogLevel = Debug | Info | Warning | Error
    deriving (Generic, Eq, Ord, Show, Read, Enum, ToJSON, FromJSON)

-- | Logging format.
type Format = String

data LoggerConfig = LoggerConfig
  { _format       :: Format
  , _level        :: LogLevel
  , _logFilePath  :: FilePath
  , _logToConsole :: Bool
  , _logToFile    :: Bool
  } deriving (Generic, Show, Read)

type Message = Text

data LogEntry = LogEntry !LogLevel !Message
type Log = [LogEntry]

standardFormat :: String
standardFormat = "$prio $loggername: $msg"

nullFormat :: String
nullFormat = "$msg"

defaultLoggerConfig :: LoggerConfig
defaultLoggerConfig = LoggerConfig
    { _format = standardFormat
    , _level = Debug
    , _logFilePath = ""
    , _logToConsole = True
    , _logToFile = False
    }

nullLoger :: LoggerConfig
nullLoger = defaultLoggerConfig
    {   _logFilePath = "null"
    ,   _logToConsole = False
    }
