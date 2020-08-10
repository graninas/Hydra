{-# LANGUAGE PackageImports #-}

module Astro.Tests.ConsoleOptionsSpec where

import           Test.Hspec (Spec, around, describe, it, shouldBe, shouldSatisfy)
import           Test.Hspec.QuickCheck (prop)

import qualified "hydra-base" Hydra.Domain  as D
import qualified "hydra-free" Hydra.Runtime as R
import qualified "hydra-free" Hydra.Interpreters as R
import qualified "hydra-free" Hydra.Testing.Functional as F

import           Hydra.Prelude
import           Astro.ConsoleOptions

import           Data.Semigroup ((<>))
import           Network.URI
import           Network.Wai.Handler.Warp (Port)
import           Options.Applicative


spec :: Spec
spec =
  describe "ConsoleOptions test" $
    it "client options test" $ do

      let pInf = info (clientOptionParser <**> helper) fullDesc
      let consoleOptsRes = execParserPure
              defaultPrefs
              pInf
              []

      case consoleOptsRes of
        Success consoleOpts -> consoleOpts `shouldBe`
            (Client (ClientOptions SH HttpChannel))
        Failure f -> fail $ "Parse failed: " <> show f
        CompletionInvoked _ -> fail "Unexpected completion invoke."
