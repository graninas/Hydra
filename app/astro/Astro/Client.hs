{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}

module Astro.Client
  ( runAstroClient
  ) where

import           Hydra.Prelude
import qualified Data.ByteString.Lazy as BSL
import           Data.Aeson (decode)
import           Data.Either (rights)

import qualified Hydra.Domain  as D
import qualified Hydra.Runtime as R
import qualified Hydra.Interpreters as R
import qualified Hydra.Language     as L

import           Astro.Common (loggerCfg)
import           Astro.Domain.Meteor
import           Astro.Domain.Asteroid
import           Astro.Types
import qualified Astro.API as API


reportAsteroid :: API.AsteroidTemplate -> L.AppL ()
reportAsteroid asteroid = undefined

reportMeteor :: API.MeteorTemplate -> L.AppL ()
reportMeteor meteor = undefined

tryParseCmd
  :: FromJSON obj
  => BSL.ByteString
  -> Either BSL.ByteString obj
tryParseCmd str = case decode str of
  Nothing -> Left "Decoding failed."
  Just obj -> Right obj

asteroidReporter = undefined
meteorReporter = undefined

reportObject
  :: FromJSON obj
  => (obj -> L.AppL ())
  -> (Either BSL.ByteString obj)
  -> L.AppL (Either BSL.ByteString ())
reportObject reporter obj = undefined

consoleApp :: L.AppL ()
consoleApp = do
  line <- L.evalIO $ BSL.putStr "> " >> BSL.getContents

  let runners =
        [ reportObject asteroidReporter $ tryParseCmd @(API.AsteroidTemplate) line
        , reportObject meteorReporter   $ tryParseCmd @(API.MeteorTemplate)   line
        ]

  eResult <- sequence runners
  case rights eResult of
    [] -> L.evalIO $ BSL.putStrLn "Command is not recognized."
    [()] -> pure ()
    (_) -> L.evalIO $ BSL.putStrLn "Multiple commands evaluated unexpectedly"

  consoleApp


runAstroClient :: IO ()
runAstroClient = R.withAppRuntime (Just loggerCfg)
    $ \rt -> R.runAppL rt consoleApp
