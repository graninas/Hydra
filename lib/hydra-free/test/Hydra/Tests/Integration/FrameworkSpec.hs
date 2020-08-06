{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PackageImports         #-}
{-# LANGUAGE TemplateHaskell        #-}

module Hydra.Tests.Integration.FrameworkSpec where

import qualified Control.Exception as E

import           Hydra.Prelude
import qualified Hydra.Domain                as D
import qualified Hydra.Language              as L
import qualified "hydra-free" Hydra.Runtime  as R
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
              void $ L.scenario $ L.runSafely @(E.AssertionFailed) $ L.throwException (E.AssertionFailed "Error")
              pure "Some"

        result <- R.runAppL rt app
        result `shouldBe` "Some"

      it "ThrowException & runSafely" $ \rt -> do
        let (app :: L.AppL (Either E.AssertionFailed Int)) =
              L.scenario $ L.runSafely $ L.throwException (E.AssertionFailed "Error")

        eRes <- R.runAppL rt app
        case eRes of
          Left (E.AssertionFailed errStr) -> errStr `shouldBe` "Error"
          Right r -> fail $ "Unexpected success: " <> show r

      it "RunSafely" $ \rt -> do
        let (app :: L.AppL (Either SomeException Int)) =
              L.scenario $ L.runSafely $ pure 10

        eRes <- R.runAppL rt app
        case eRes of
          Left e -> fail $ "Unexpected failure: " <> show e
          Right n -> n `shouldBe` 10
