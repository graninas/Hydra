{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}

module Astro.Config where

import           Hydra.Prelude
import qualified Hydra.Domain  as D
import qualified Database.Beam.Sqlite as BS

-- TODO: configs from the command line
dbConfig :: D.DBConfig BS.SqliteM
dbConfig = D.mkSQLiteConfig "/tmp/astro.db"

loggerCfg :: D.LoggerConfig
loggerCfg = D.LoggerConfig
  { D._format       = "$prio $loggername: $msg"
  , D._level        = D.Debug
  , D._logFilePath  = ""
  , D._logToConsole = True
  , D._logToFile    = False
  }
