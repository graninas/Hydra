{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import           Hydra.Prelude
import           System.Environment (getArgs)

import           Astro.Server (runAstroServer)
import           Astro.Client (runAstroClient)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("client":_) -> runAstroClient
    _ -> runAstroServer
