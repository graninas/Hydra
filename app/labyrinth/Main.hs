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


initGameState :: IO GameState
initGameState = do

  pure $ GameState Map.empty


main :: IO ()
main = do
  gameState <- initGameState

  R.withAppRuntime (Just loggerCfg) (\rt -> runReaderT (R.runAppL rt app) gameState)
