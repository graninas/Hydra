
module Labyrinth.Tests.KVDBSpec where

import qualified Control.Exception as E
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Word as Word8
import           Unsafe.Coerce (unsafeCoerce)
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
  describe "KV DB functional tests" $
    it "load game failure test" $ do

      mocks <- newIORef
        [ F.Mock $ unsafeCoerce $ D.StateVar 0   -- renderTemplateVar
        , F.Mock $ unsafeCoerce $ D.StateVar 1   -- renderVar
        , F.Mock $ unsafeCoerce $ D.StateVar 2   -- labVar
        , F.Mock $ unsafeCoerce $ D.StateVar 3   -- labBoundsVar
        , F.Mock $ unsafeCoerce $ D.StateVar 4   -- wormholesVar
        , F.Mock $ unsafeCoerce $ D.StateVar 5   -- posVar
        , F.Mock $ unsafeCoerce $ D.StateVar 6   -- playerHPVar
        , F.Mock $ unsafeCoerce $ D.StateVar 7   -- bearPosVar
        , F.Mock $ unsafeCoerce $ D.StateVar 8   -- tr
        , F.Mock $ unsafeCoerce $ D.StateVar 9   -- gameStateVar
        , F.Mock $ unsafeCoerce $ D.StateVar 10  -- moveMsgsVar

        , F.Mock $ unsafeCoerce $ Left $ D.DBError SystemError "KVDB Failure."
        ]

      let testRt = F.TestRuntime Nothing mocks
      strRes <- F.runAppL testRt $ do
        st <- initAppState False (0, 0) 100 (0, 0) testLabyrinth1 PlayerMove testKvdbConfig
        loadGame st 0
      strRes `shouldBe` "Game succesfully loaded from KV DB."
