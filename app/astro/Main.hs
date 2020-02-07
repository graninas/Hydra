{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import           Hydra.Prelude
import           System.Environment (getArgs)

import           Astro.Server (runAstroServer)
import           Astro.Client (ReportChannel(..), runAstroClient)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("http_client":_) -> runAstroClient HttpChannel
    ("tcp_client":_)  -> runAstroClient TcpChannel
    _ -> runAstroServer
