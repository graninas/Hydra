module Main where

import           Labyrinth.Prelude

import qualified Hydra.Domain               as D
import qualified Hydra.Runtime              as R
import qualified Hydra.Interpreters         as R

import           Labyrinth.App (app)
import           Labyrinth.Types
import           Labyrinth.Domain
import           Labyrinth.Render
import           Labyrinth.Labyrinths
import           Labyrinth.Algorithms

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
  let LabyrinthInfo {..} = analyzeLabyrinth lab
  let renderTemplate = renderSkeleton _bounds

  renderTemplateVar <- newVarIO renderTemplate
  labRenderVar      <- newVarIO renderTemplate
  labVar            <- newVarIO lab
  labBoundsVar      <- newVarIO _bounds
  wormholesVar      <- newVarIO _wormholes
  posVar            <- newVarIO (0, 0)
  playerHPVar       <- newVarIO 100
  bearPosVar        <- newVarIO (0, 0)
  inv               <- Inventory <$> newVarIO False
  gameStateVar      <- newVarIO GameStart
  moveMsgsVar       <- newVarIO []

  pure $ AppState
    labVar
    labBoundsVar
    renderTemplateVar
    labRenderVar
    wormholesVar
    posVar
    playerHPVar
    bearPosVar
    inv
    gameStateVar
    moveMsgsVar

startApp :: AppL ()
startApp = initAppState testLabyrinth2 >>= app

execApp :: Maybe D.LoggerConfig -> AppL a -> IO a
execApp mbCfg act = R.withAppRuntime mbCfg $ \rt -> R.runAppL rt act

main :: IO ()
main =
  execApp (Just loggerCfg) startApp
