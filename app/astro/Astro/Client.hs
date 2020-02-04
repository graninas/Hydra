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

import qualified Hydra.Domain  as D
import qualified Hydra.Runtime as R
import qualified Hydra.Interpreters as R
import qualified Hydra.Language     as L

import           Astro.Domain.Meteor
import           Astro.Domain.Asteroid
import           Astro.API.Meteor
import           Astro.API.Asteroid
import           Astro.Types


reportAsteroid :: API.AsteroidTemplate -> Flow ()
reportAsteroid asteroid = undefined

reportMeteor :: API.MeteorTemplate -> Flow ()
reportMeteor meteor = undefined

tryParseCmd :: BSL.BytetString -> Either BSL.BytetString obj
tryParseCmd str = case decode str of
  Nothing -> Left "Decoding failed."
  Just obj -> Right obj



reportObject :: (obj -> Flow ()) -> obj -> Flow (Either BS.ByteString ())
reportObject reporter obj = undefined

consoleFlow :: Flow ()
consoleFlow = do
  line <- L.runIO $ BSL.putStr "> " >> BSL.getLine

  let runners =
        [ reportObject asteroidReporter $ tryParseCmd @AsteroidTemplate line
        , reportObject meteorReporter   $ tryParseCmd @MeteorTemplate   line
        ]

  eResult <- sequence runners
  case eResult of
    Left err -> L.runIO $ BSL.putStrLn $ "Command failed: " <> err
    Right _ -> pure ()

  consoleFlow

  case parseCmd line of
    Nothing -> do
      L.runIO $ BSL.putStrLn "Command not recognized."
      consoleFlow
    Just asteroid@(API.AsteroidTemplate {}) -> reportAsteroid asteroid
    Just meteor@(API.MeteorTemplate {})     -> reportMeteor meteor


runAstroClient :: IO ()
runAstroClient = do

    R.withAppRuntime (Just loggerCfg) $ \rt -> do
      appSt <- R.runAppL rt $ initState AppConfig
      run 8080 $ astroBackendApp $ Env rt appSt
