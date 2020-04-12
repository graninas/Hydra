module Main where

import           Hydra.Prelude

import           System.Environment         (getArgs)
import qualified Data.Text                  as T
import qualified Data.Map                   as Map

import qualified Hydra.Domain               as D
import qualified Hydra.Language             as L
import qualified Hydra.Runtime              as R
import qualified Hydra.Interpreters         as R

import           Labyrinth.App (app)
import           Labyrinth.Types
import           Labyrinth.Domain

loggerCfg :: D.LoggerConfig
loggerCfg = D.LoggerConfig
  { D._format       = "$prio $loggername: $msg"
  , D._level        = D.Debug
  , D._logFilePath  = ""
  , D._logToConsole = False
  , D._logToFile    = False
  }


initGameState :: L.AppL GameState
initGameState = pure $ GameState Map.empty

startApp :: L.AppL ()
startApp = do
  st <- initGameState
  app st

main :: IO ()
main = R.withAppRuntime (Just loggerCfg) (\rt -> R.runAppL rt startApp)
