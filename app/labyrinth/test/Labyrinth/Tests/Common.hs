{-# LANGUAGE PackageImports #-}

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
import qualified "hydra-free" Hydra.Runtime as R
import qualified Hydra.Interpreters         as R

import           Labyrinth.Prelude
import qualified Labyrinth.KVDB.Model as KVDB
import qualified Labyrinth.KVDB.Repository as KVDB
import           Labyrinth


testKvdbConfig :: KVDBConfig KVDB.LabKVDB
testKvdbConfig = RocksDBConfig "./test/Labyrinth/TestData/labyrinths.rdb" True False

withAppState :: Labyrinth -> R.AppRuntime -> (AppState -> IO a) -> IO a
withAppState lab rt act = do
  st <- R.runAppL rt $ initAppState False (0, 0) 100 (0, 0) lab PlayerMove testKvdbConfig
  act st

withLabyrinthApp :: Labyrinth -> ((R.AppRuntime, AppState) -> IO a) -> IO a
withLabyrinthApp lab act
  = R.withAppRuntime Nothing
  $ \rt -> withAppState lab rt
  $ \st -> act (rt, st)

runLabMethod :: PlayerPos -> Labyrinth -> R.AppRuntime -> (AppState -> AppL a) -> IO a
runLabMethod plPos lab rt act = R.runAppL rt $ do
  st <- initAppState False plPos 100 (0, 0) lab PlayerMove testKvdbConfig
  act st

runLabMethodWithTreasure :: PlayerPos -> Labyrinth -> R.AppRuntime -> (AppState -> AppL a) -> IO a
runLabMethodWithTreasure plPos lab rt act = R.runAppL rt $ do
  st <- initAppState True plPos 100 (0, 0) lab PlayerMove testKvdbConfig
  act st
