module Labyrinth.Tests.Common where

import qualified Control.Exception as E
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Word as Word8
import           Test.Hspec (Spec, around, describe, it, shouldBe, shouldSatisfy)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (arbitrary, property, verbose, withMaxSuccess)
import           Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run)

import qualified Hydra.Domain               as D
import qualified Hydra.Runtime              as R
import qualified Hydra.Interpreters         as R

import           Labyrinth.Prelude
import qualified Labyrinth.KVDB.Model as KVDB
import qualified Labyrinth.KVDB.Repository as KVDB
import           Labyrinth


testKvdbConfig :: KVDBConfig KVDB.LabKVDB
testKvdbConfig = RocksDBConfig "./labyrinths/" True False


initAppState :: Bool -> (Int, Int, Labyrinth) -> AppL AppState
initAppState hasTreasure (x0, y0, lab) = do
  let LabyrinthInfo {liBounds, liWormholes} = analyzeLabyrinth lab
  let renderTemplate = renderSkeleton liBounds

  renderTemplateVar <- newVarIO renderTemplate
  labRenderVar      <- newVarIO renderTemplate
  labVar            <- newVarIO lab
  labBoundsVar      <- newVarIO liBounds
  wormholesVar      <- newVarIO liWormholes
  posVar            <- newVarIO (x0, y0)
  playerHPVar       <- newVarIO 100
  bearPosVar        <- newVarIO (x0, y0)
  inv               <- InventoryState <$> newVarIO hasTreasure
  gameStateVar      <- newVarIO PlayerMove
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
    testKvdbConfig

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
