{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PackageImports         #-}
{-# LANGUAGE TemplateHaskell        #-}

module Hydra.Tests.Integration.FrameworkSpec where

import qualified Control.Exception as E

import           Hydra.Prelude
import qualified Hydra.Domain                as D
import qualified Hydra.Language              as L
import qualified Hydra.Runtime               as R
import qualified Hydra.Interpreters          as R
import           Hydra.Testing.Integrational
import           Hydra.Testing.Wrappers
import           Test.Hspec

import           Hydra.TestData


spec :: Spec
spec =
  around (R.withAppRuntime Nothing) $ do
    describe "Methods" $ do

      it "ThrowException not catched" $ \rt -> do
        let app = do
              L.scenario $ L.throwException (E.AssertionFailed "Error")
              pure "Some"

        result <- E.catch (R.runAppL rt app) (\e -> pure $ show (e :: E.AssertionFailed))
        result `shouldBe` "Error"

      it "ThrowException catched" $ \rt -> do
        let app = do
              void $ L.scenario $ L.runSafely $ L.throwException (E.AssertionFailed "Error")
              pure "Some"

        result <- R.runAppL rt app
        result `shouldBe` "Some"

      it "ThrowException & runSafely" $ \rt -> do
        let (app :: L.AppL (Either Text Int)) =
              L.scenario $ L.runSafely $ L.throwException (E.AssertionFailed "Error")

        result <- R.runAppL rt app
        result `shouldBe` (Left "Error")

      it "RunSafely" $ \rt -> do
        let (app :: L.AppL (Either Text Int)) =
              L.scenario $ L.runSafely $ pure 10

        result <- R.runAppL rt app
        result `shouldBe` (Right 10)
