{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PackageImports         #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE DeriveAnyClass        #-}

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

data TestRequest = TestRequest Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data TestResponse = TestResponse Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance D.Rpc TestRequest TestResponse

acceptTestRequest :: TestRequest -> L.LangL TestResponse
acceptTestRequest (TestRequest echo) = pure $ TestResponse echo

jsonRpcServer :: L.AppL (Either D.NetworkError TestResponse)
jsonRpcServer = do
  void
    $ L.serveRpc 5555
    $ L.rpcMethod acceptTestRequest

  L.delay 1000000

  val <- L.scenario
    $ L.callRPC (D.Address "127.0.0.1" 5555)
    $ TestRequest "Hi!"

  L.delay 1000000

  pure val


spec :: Spec
spec =
  around (R.withAppRuntime Nothing) $ do
    describe "Methods" $ do

      it "Run JSON-RPC Server and make JSON-RPC request" $ \rt -> do
        eResult <- R.runAppL rt jsonRpcServer
        case eResult of
          Left err -> fail $ show err
          Right (TestResponse t) -> t `shouldBe` "Hi!"
      -- it "Run JSON-RPC Server and make JSON-RPC request" $ \rt -> do
      --   eResult <- R.runAppL rt jsonRpcServer
      --   case eResult of
      --     Left err -> fail ""
      --     Right (TestResponse t) -> t `shouldBe` "Hi!"

      it "ThrowException not catched" $ \rt -> do
        let app = do
              L.scenario $ L.throwException (E.AssertionFailed "Error")
              pure "Some"

        result <- E.catch (R.runAppL rt app) (\e -> pure $ show (e :: E.AssertionFailed))
        result `shouldBe` "Error"

      it "ThrowException & runSafely, catched exactly" $ \rt -> do
        let app = do
              void $ L.scenario $ L.runSafely @(E.AssertionFailed) $ L.throwException (E.AssertionFailed "Error")
              pure "Some"
        result <- R.runAppL rt app
        result `shouldBe` "Some"

      it "ThrowException & runSafely, not catched different" $ \rt -> do
        let app = do
              void $ L.scenario $ L.runSafely @(E.AssertionFailed) $ L.throwException E.DivideByZero
              pure "Some"

        eRes :: Either SomeException String <- try $ R.runAppL rt app
        case eRes of
          Left err -> show err `shouldBe` "divide by zero"
          Right res -> fail $ "Unexpected success: " <> res

      it "ThrowException & runSafely, catched wider" $ \rt -> do
        let app = do
              void $ L.scenario $ L.runSafely @SomeException $ L.throwException (E.AssertionFailed "Error")
              pure "Some"
        result <- R.runAppL rt app
        result `shouldBe` "Some"

      it "ThrowException & runSafely, catched by second, wider" $ \rt -> do
        let app = do
              void $ L.scenario
                $ L.runSafely @SomeException        -- by second
                $ L.runSafely @(E.AssertionFailed)  -- by first
                $ L.throwException E.DivideByZero
              pure "Some"
        result <- R.runAppL rt app
        result `shouldBe` "Some"

      it "ThrowException & runSafely, not catched any" $ \rt -> do
        let app = do
              void $ L.scenario
                $ L.runSafely @(E.AssertionFailed)    -- second
                $ L.runSafely @(E.ArrayException)     -- first
                $ L.throwException E.DivideByZero
              pure "Some"
        eRes :: Either SomeException String <- try $ R.runAppL rt app
        case eRes of
          Left err -> show err `shouldBe` "divide by zero"
          Right res -> fail $ "Unexpected success: " <> res

      it "ThrowException & runSafely', catched by second, wider" $ \rt -> do
        let app = do
              void $ L.scenario
                $ L.runSafely'                        -- second (SomeException)
                $ L.runSafely @(E.AssertionFailed)    -- first
                $ L.throwException E.DivideByZero
              pure "Some"
        result <- R.runAppL rt app
        result `shouldBe` "Some"

      it "ThrowException & runSafely, catched after run" $ \rt -> do
        let (app :: L.AppL (Either E.AssertionFailed Int)) =
              L.scenario $ L.runSafely $ L.throwException (E.AssertionFailed "Error")

        eRes <- R.runAppL rt app
        case eRes of
          Left (E.AssertionFailed errStr) -> errStr `shouldBe` "Error"
          Right r -> fail $ "Unexpected success: " <> show r

      it "RunSafely, no exceptions" $ \rt -> do
        let (app :: L.AppL (Either SomeException Int)) =
              L.scenario $ L.runSafely $ pure 10

        eRes <- R.runAppL rt app
        case eRes of
          Left e -> fail $ "Unexpected failure: " <> show e
          Right n -> n `shouldBe` 10
