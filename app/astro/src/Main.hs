{-# LANGUAGE PackageImports #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Hydra.Prelude

import qualified Hydra.Runtime              as R
import qualified "hydra-free" Hydra.Interpreters         as R

import           Astro.Config               (loggerCfg)
import           Astro.ConsoleOptions
import           Astro.Server               (runAstroServer)
import           Astro.Client.Common        (ReportChannel(..), DIApproach(..))
import qualified Astro.Client.ServiceHandle as SH
import qualified Astro.Client.ReaderT       as RT
import qualified Astro.Client.FreeMonad     as FM
import qualified Astro.Client.FinalTagless  as FT
import qualified Astro.Client.FinalTagless2 as FT2
import qualified Astro.Client.GADT          as GADT

runAstroClient :: ClientOptions -> IO ()
runAstroClient (ClientOptions appr ch)
    = R.withAppRuntime (Just loggerCfg) (\rt -> R.runAppL rt app')
  where
    app' = app'' appr ch

    -- Selector of a DI approach.
    app'' SH   _ = SH.consoleApp $ SH.makeServiceHandle ch
    app'' RT   _ = runReaderT RT.consoleApp $ RT.makeAppEnv ch
    app'' FM   _ = FM.consoleApp $ FM.getAstroServiceRunner ch
    app'' GADT _ = GADT.consoleApp $ GADT.getAstroServiceRunner ch
    app'' FT   HttpChannel = FT.consoleApp @(FT.HttpAstroService)
    app'' FT   TcpChannel  = FT.consoleApp @(FT.TcpAstroService)
    app'' FT2  HttpChannel = FT2.consoleApp @(FT2.HttpAstroService)
    app'' FT2  TcpChannel  = FT2.consoleApp @(FT2.TcpAstroService)
    app'' _ _    = error $ "Approach not yet implemented: " <> show appr

main :: IO ()
main = do
  (ConsoleOptions cmd) <- parseConsoleOptions
  case cmd of
    Client cliOpts -> runAstroClient cliOpts
    Server serOpts -> runAstroServer serOpts
