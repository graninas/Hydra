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

-- l r u d
testLabyrinth2 :: Labyrinth
testLabyrinth2 = Map.fromList
  [ ((0, 0), (Cell (Monolith False) Wall (Monolith False) NoWall, NoContent))
  , ((1, 0), (Cell Wall NoWall (Monolith False) NoWall, NoContent))
  , ((2, 0), (Cell NoWall (Monolith False) (Monolith False) NoWall, (Wormhole 0)))

  , ((0, 1), (Cell (Monolith False) NoWall NoWall NoWall, NoContent))
  , ((1, 1), (Cell NoWall Wall NoWall Wall, Treasure))
  , ((2, 1), (Cell Wall (Monolith True) NoWall NoWall, NoContent))

  , ((0, 2), (Cell (Monolith False) NoWall NoWall (Monolith False), (Wormhole 1)))
  , ((1, 2), (Cell NoWall NoWall Wall (Monolith False), NoContent))
  , ((2, 2), (Cell NoWall (Monolith False) NoWall (Monolith False), NoContent))
  ]

testLabyrinth1 :: Labyrinth
testLabyrinth1 = Map.fromList
  [ ((0, 0), (Cell (Monolith False) (Monolith False) (Monolith False) (Monolith True), Treasure))
  ]

analyzeLabyrinth :: Labyrinth -> (Bounds, Wormholes, Labyrinth)
analyzeLabyrinth lab =
  let (b, w) = Map.foldrWithKey f ((0, 0), Map.empty) lab
  in (b, w, lab)
  where
    increaseBounds :: Bounds -> Pos -> Bounds
    increaseBounds (x', y') (x, y) = (max x' (x + 1), max y' (y + 1))
    f :: Pos -> (Cell, Content) -> (Bounds, Wormholes) -> (Bounds, Wormholes)
    f pos (_, Wormhole n) (bounds, wormholes) = (increaseBounds bounds pos, Map.insert n pos wormholes)
    f pos _ (bounds, wormholes) = (increaseBounds bounds pos, wormholes)

initGameState :: AppL GameState
initGameState = do
  let (bounds, wormholes, lab) = analyzeLabyrinth testLabyrinth2
  let renderTemplate = renderSkeleton bounds

  labRenderVar     <- newVarIO renderTemplate
  labVar           <- newVarIO lab
  labSizeVar       <- newVarIO bounds
  posVar           <- newVarIO (2, 2)
  inv              <- Inventory <$> newVarIO False
  aboutLeavingVar  <- newVarIO Nothing
  finishedVar      <- newVarIO False
  moveMsgsVar      <- newVarIO []

  pure $ GameState
    labVar
    labSizeVar
    renderTemplate
    labRenderVar
    wormholes
    posVar
    inv
    aboutLeavingVar
    finishedVar
    moveMsgsVar


startApp :: AppL ()
startApp = initGameState >>= app

main :: IO ()
main = R.withAppRuntime (Just loggerCfg) (\rt -> R.runAppL rt startApp)
