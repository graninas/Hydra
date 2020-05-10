
module Labyrinth.Tests.LogicSpec where

import qualified Control.Exception as E

import           Labyrinth.Prelude
import           Labyrinth.App
import           Labyrinth.Types
import           Labyrinth.Domain
import           Labyrinth.Render
import           Labyrinth.Labyrinths
import           Labyrinth.Gen
import           Labyrinth.Lens

import           Test.Hspec
import qualified Hydra.Domain               as D
import qualified Hydra.Runtime              as R
import qualified Hydra.Interpreters         as R


initAppState :: Bool -> (Int, Int, Labyrinth) -> AppL AppState
initAppState hasTreasure (x0, y0, lab) = do
  let (bounds, wormholes) = analyzeLabyrinth lab
  let renderTemplate = renderSkeleton bounds

  labRenderVar     <- newVarIO renderTemplate
  labVar           <- newVarIO lab
  labSizeVar       <- newVarIO bounds
  posVar           <- newVarIO (x0, y0)
  inv              <- Inventory <$> newVarIO hasTreasure
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
  st <- R.runAppL rt $ initAppState False (0, 0, lab)
  act st

withLabyrinthApp :: Labyrinth -> ((R.AppRuntime, AppState) -> IO a) -> IO a
withLabyrinthApp lab act
  = R.withAppRuntime Nothing
  $ \rt -> withAppState lab rt
  $ \st -> act (rt, st)

runLabMethod :: (Int, Int, Labyrinth) -> R.AppRuntime -> (AppState -> AppL a) -> IO a
runLabMethod startLab rt act = R.runAppL rt (initAppState False startLab >>= act)

runLabMethodWithTreasure :: (Int, Int, Labyrinth) -> R.AppRuntime -> (AppState -> AppL a) -> IO a
runLabMethodWithTreasure startLab rt act = R.runAppL rt (initAppState True startLab >>= act)

spec :: Spec
spec =
  around (R.withAppRuntime Nothing) $ do

    describe "testMove tests" $ do

      it "testMove DirUp monolith" $ \rt -> do
        movingResult <- runLabMethod (0, 0, testLabyrinth2) rt (\st -> scenario $ testMove st DirUp)
        movingResult `shouldBe` ImpossibleMove "Step impossible: monolith wall"

      it "testMove DirRight wall" $ \rt -> do
        movingResult <- runLabMethod (0, 0, testLabyrinth2) rt (\st -> scenario $ testMove st DirRight)
        movingResult `shouldBe` ImpossibleMove "Step impossible: wall"

      it "testMove DirDown pass" $ \rt -> do
        movingResult <- runLabMethod (0, 0, testLabyrinth2) rt (\st -> scenario $ testMove st DirDown)
        movingResult `shouldBe` (SuccessfullMove (0,1) (Cell (Monolith False) NoWall NoWall NoWall) NoContent)

      it "testMove DirRight treasure" $ \rt -> do
        movingResult <- runLabMethod (0, 1, testLabyrinth2) rt (\st -> scenario $ testMove st DirRight)
        movingResult `shouldBe` (SuccessfullMove (1,1) (Cell NoWall Wall NoWall Wall) Treasure)

      it "testMove DirDown wormhole" $ \rt -> do
        movingResult <- runLabMethod (0, 1, testLabyrinth2) rt (\st -> scenario $ testMove st DirDown)
        movingResult `shouldBe` (SuccessfullMove (0,2) (Cell (Monolith False) NoWall NoWall (Monolith False)) (Wormhole 1))

      it "testMove DirRight exit no treasure" $ \rt -> do
        movingResult <- runLabMethod (2, 1, testLabyrinth2) rt (\st -> scenario $ testMove st DirRight)
        movingResult `shouldBe` (ExitFound False)

      it "testMove DirRight exit treasure" $ \rt -> do
        movingResult <- runLabMethodWithTreasure (2, 1, testLabyrinth2) rt (\st -> scenario $ testMove st DirRight)
        movingResult `shouldBe` (ExitFound True)

    describe "performPlayerContentEvent tests" $ do

      it "performPlayerContentEvent no content" $ \rt -> do
        (pos, tr, gs) <- runLabMethod (0, 0, testLabyrinth2) rt (\st -> do
          scenario $ performPlayerContentEvent st

          pos <- readVarIO $ st ^. playerPos
          tr  <- readVarIO $ st ^. playerInventory . treasure
          gs  <- readVarIO $ st ^. gameState
          pure (pos, tr, gs))

        pos `shouldBe` (0, 0)
        tr `shouldBe` False
        gs `shouldBe` PlayerMove

      it "performPlayerContentEvent wormhole" $ \rt -> do
        (pos, tr, gs) <- runLabMethod (0, 2, testLabyrinth2) rt (\st -> do
          scenario $ performPlayerContentEvent st

          pos <- readVarIO $ st ^. playerPos
          tr  <- readVarIO $ st ^. playerInventory . treasure
          gs  <- readVarIO $ st ^. gameState
          pure (pos, tr, gs))

        pos `shouldBe` (2, 0)
        tr `shouldBe` False
        gs `shouldBe` PlayerMove
