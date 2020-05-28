
module Labyrinth.Tests.KVDBSpec where

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
import qualified Hydra.Testing.Functional   as F

import           Labyrinth.Prelude
import           Labyrinth
import           Labyrinth.Tests.Common

spec :: Spec
spec =
  describe "KV DB tests" $
    it "load game test" $ do
      evalLangMocks <- newIORef []
      let testRt = F.TestRuntime evalLangMocks
      strRes <- F.runAppL testRt $ do
        st <- initAppState False (0, 0) 100 (0, 0) testLabyrinth1 PlayerMove testKvdbConfig
        loadGame st 0
      strRes `shouldBe` "Game succesfully loaded from KV DB."
