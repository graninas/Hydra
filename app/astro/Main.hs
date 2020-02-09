{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Hydra.Prelude
import           System.Environment         (getArgs)

import qualified Hydra.Runtime              as R
import qualified Hydra.Interpreters         as R

import           Astro.Config               (loggerCfg)
import           Astro.Server               (runAstroServer)
import           Astro.Client.Common        (ReportChannel(..), Approach(..))
import qualified Astro.Client.ServiceHandle as SH
import qualified Astro.Client.ReaderT       as RT
import qualified Astro.Client.FreeMonad     as FM
import qualified Astro.Client.GADT          as GADT


runAstroClient :: Approach -> ReportChannel -> IO ()
runAstroClient appr ch = R.withAppRuntime (Just loggerCfg) (\rt -> R.runAppL rt app')
  where
    app' = app'' appr

    app'' SH   = SH.consoleApp $ SH.makeServiceHandle ch
    app'' RT   = runReaderT RT.consoleApp $ RT.makeAppEnv ch
    app'' FM   = FM.consoleApp $ FM.getAstroServiceRunner ch
    app'' GADT = GADT.consoleApp $ GADT.getAstroServiceRunner ch
    app'' _    = error $ "Approach not yet implemented: " <> show appr

getChannel :: String -> ReportChannel
getChannel "http" = HttpChannel
getChannel "tcp"  = TcpChannel
getChannel ch     = error $ show $ "Channel not supported: " <> ch <> " Supported: http tcp"

getApproach :: String -> Approach
getApproach apprStr = case readMaybe apprStr of
  Just appr -> appr
  Nothing   -> error $ show $ "Approach not supported: " <> apprStr <> " Supported: SH RT"

main :: IO ()
main = do
  args <- getArgs
  case args of
    (chan : appr : _) -> runAstroClient (getApproach appr) (getChannel chan)
    ("client" : _)    -> runAstroClient SH HttpChannel
    ("server" : _)    -> runAstroServer
    _                 -> putStrLn @String "Args not recognized."
