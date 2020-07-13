module Main where

import           Labyrinth.Prelude

import qualified Hydra.Domain               as D
import qualified Hydra.Runtime              as R
import qualified Hydra.Interpreters         as R

import qualified Labyrinth as Lab
import           Labyrinth.KVDB.Model (LabKVDB)

kvdbConfig :: KVDBConfig LabKVDB
kvdbConfig = RocksDBConfig "./app/labyrinth/" True False

loggerCfg :: D.LoggerConfig
loggerCfg = D.LoggerConfig
  { D._format       = "$prio $loggername: $msg"
  , D._level        = D.Debug
  , D._logFilePath  = ""
  , D._logToConsole = False
  , D._logToFile    = False
  }

startApp :: AppL ()
startApp = do
  st <- Lab.initAppState False (0,0) 100 (0,0) Lab.testLabyrinth2 Lab.GameStart kvdbConfig
  Lab.labyrinthApp st

main :: IO ()
main = R.withAppRuntime (Just loggerCfg)
  $ \rt -> R.runAppL rt startApp
