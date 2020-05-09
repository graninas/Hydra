module Main where

import           Labyrinth.Prelude

import qualified Data.Map                   as Map

import qualified Hydra.Domain               as D
import qualified Hydra.Runtime              as R
import qualified Hydra.Interpreters         as R

import           Labyrinth.App (app)
import           Labyrinth.Types
import           Labyrinth.Domain
import           Labyrinth.Render
import           Labyrinth.Labyrinths
import           Labyrinth.Gen

loggerCfg :: D.LoggerConfig
loggerCfg = D.LoggerConfig
  { D._format       = "$prio $loggername: $msg"
  , D._level        = D.Debug
  , D._logFilePath  = ""
  , D._logToConsole = False
  , D._logToFile    = False
  }

initAppState :: Labyrinth -> AppL AppState
initAppState lab = do
  let (bounds@(x, y), wormholes) = analyzeLabyrinth lab
  let renderTemplate = renderSkeleton bounds

  labRenderVar     <- newVarIO renderTemplate
  labVar           <- newVarIO lab
  labSizeVar       <- newVarIO bounds
  posVar           <- newVarIO (0, 0)
  inv              <- Inventory <$> newVarIO False
  gameStateVar     <- newVarIO PlayerMove
  moveMsgsVar      <- newVarIO []

  pure $ AppState
    labVar
    labSizeVar
    renderTemplate
    labRenderVar
    wormholes
    posVar
    inv
    gameStateVar
    moveMsgsVar

startApp :: AppL ()
startApp = initAppState testLabyrinth2 >>= app

main :: IO ()
main = R.withAppRuntime (Just loggerCfg) (\rt -> R.runAppL rt startApp)
