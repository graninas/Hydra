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

testLabyrinth = Map.fromList
  [ ((0, 0), (Cell Wall Wall Wall Wall, Treasure))
  ]


initGameState :: L.AppL GameState
initGameState = do
  lab       <- L.newVarIO testLabyrinth
  labSize   <- L.newVarIO (1, 1)
  pos       <- L.newVarIO (0, 0)
  inv       <- Inventory <$> L.newVarIO False
  treasure  <- L.newVarIO Nothing
  fiinished <- L.newVarIO False
  pure $ GameState lab labSize Map.empty pos inv treasure fiinished



startApp :: L.AppL ()
startApp = initGameState >>= app

main :: IO ()
main = R.withAppRuntime (Just loggerCfg) (\rt -> R.runAppL rt startApp)
