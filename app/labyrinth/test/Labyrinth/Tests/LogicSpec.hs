
module Labyrinth.Tests.LogicSpec where

import qualified Control.Exception as E

import           Labyrinth.Prelude
import           Labyrinth.App
import           Labyrinth.Types
import           Labyrinth.Domain
import           Labyrinth.Render
import           Labyrinth.Labyrinths
import           Labyrinth.Gen

import           Test.Hspec
import qualified Hydra.Domain               as D
import qualified Hydra.Runtime              as R
import qualified Hydra.Interpreters         as R


initAppState :: Labyrinth -> AppL AppState
initAppState lab = do
  let (bounds@(x, y), wormholes) = analyzeLabyrinth lab
  let renderTemplate = renderSkeleton bounds

  labRenderVar     <- newVarIO renderTemplate
  labVar           <- newVarIO lab
  labSizeVar       <- newVarIO bounds
  posVar           <- newVarIO (x - 1, y - 1)
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


withAppState :: Labyrinth -> R.AppRuntime -> (AppState -> IO a) -> IO a
withAppState lab rt act = do
  st <- R.runAppL rt $ initAppState lab
  act st

withLabyrinthApp :: Labyrinth -> ((R.AppRuntime, AppState) -> IO a) -> IO a
withLabyrinthApp lab act
  = R.withAppRuntime Nothing
  $ \rt -> withAppState lab rt
  $ \st -> act (rt, st)


spec :: Spec
spec =
  around (withLabyrinthApp testLabyrinth1) $
    describe "Labyrinth handlers" $ do

      it "Test move" $ \(rt, st) -> do

        movingResult <- R.runAppL rt $ scenario $ testMove st DirUp
        movingResult `shouldBe` ImpossibleMove "Step impossible: monolith wall"
