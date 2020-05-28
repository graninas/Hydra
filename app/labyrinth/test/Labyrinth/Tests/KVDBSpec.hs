
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
import qualified Hydra.Testing.Functional   as FT

import           Labyrinth.Prelude
import           Labyrinth
import           Labyrinth.Tests.Common

spec :: Spec
spec =
  describe "KV DB tests" $ do
    it "load game test" $ do
      loadGame
