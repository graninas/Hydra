module Main where

import           Labyrinth.Prelude

import           System.Environment         (getArgs)
import qualified Data.Map                   as Map

import qualified Hydra.Domain               as D
import qualified Hydra.Runtime              as R
import qualified Hydra.Interpreters         as R

import           Labyrinth.App (app)
import           Labyrinth.Types
import           Labyrinth.Domain
import           Labyrinth.Render

loggerCfg :: D.LoggerConfig
loggerCfg = D.LoggerConfig
  { D._format       = "$prio $loggername: $msg"
  , D._level        = D.Debug
  , D._logFilePath  = ""
  , D._logToConsole = False
  , D._logToFile    = False
  }

testLabyrinth2 :: (Pos, Labyrinth)
testLabyrinth2 = ((3, 3),) $ Map.fromList
  [ ((0, 0), (Cell (Monolith False) Wall Wall Wall, NoContent))
  , ((1, 0), (Cell (Monolith False) Wall NoWall Wall, Treasure))
  , ((2, 0), (Cell (Monolith False) Wall Wall Wall, NoContent))

  , ((0, 1), (Cell NoWall Wall Wall Wall, NoContent))
  , ((1, 1), (Cell NoWall NoWall NoWall NoWall, NoContent))
  , ((2, 1), (Cell Wall NoWall Wall Wall, NoContent))

  , ((0, 2), (Cell Wall (Monolith False) Wall Wall, NoContent))
  , ((1, 2), (Cell Wall (Monolith False) Wall NoWall, NoContent))
  , ((2, 2), (Cell Wall (Monolith False) Wall Wall, NoContent))
  ]

testLabyrinth1 :: (Pos, Labyrinth)
testLabyrinth1 = ((1, 1),) $ Map.fromList
  [ ((0, 0), (Cell (Monolith False) (Monolith False) (Monolith False) (Monolith True), Treasure))
  ]

initGameState :: AppL GameState
initGameState = do
  let (bounds, lab) = testLabyrinth1
  let wormholes = Map.empty
  let renderTemplate = renderSkeleton bounds

  labRenderVar     <- newVarIO renderTemplate
  labVar           <- newVarIO lab
  labSizeVar       <- newVarIO bounds
  posVar           <- newVarIO (0, 0)
  inv              <- Inventory <$> newVarIO False
  aboutLeaving     <- newVarIO Nothing
  finished         <- newVarIO False

  pure $ GameState labVar labSizeVar renderTemplate labRenderVar wormholes posVar inv aboutLeaving finished


startApp :: AppL ()
startApp = initGameState >>= app

main :: IO ()
main = R.withAppRuntime (Just loggerCfg) (\rt -> R.runAppL rt startApp)
