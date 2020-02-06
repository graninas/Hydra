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
import           Servant.Client (ClientM, ClientError, BaseUrl)

import qualified Hydra.Domain  as D
import qualified Hydra.Runtime as R
import qualified Hydra.Interpreters as R
import qualified Hydra.Language     as L

import           Astro.Common (loggerCfg)
import           Astro.Domain.Meteor
import           Astro.Domain.Asteroid
import           Astro.Types
import qualified Astro.Server as Server
import qualified Astro.API as API

data TcpConn = DummyTcpConn

meteors :: Maybe Int -> Maybe Int -> ClientM Meteors
meteor :: API.MeteorTemplate -> ClientM MeteorId
asteroid :: API.AsteroidTemplate -> ClientM AsteroidId
(meteors :<|> meteor :<|> asteroid) = client Server.astroAPI

meteorHttpReporter :: BaseUrl -> API.MeteorTemplate -> L.AppL (Either String MeteorId)
meteorHttpReporter url m = do
  eMId <- L.callAPI url $ meteor m
  pure $ case eMId of
    Left err -> Left $ show err
    Right r -> Right r

asteroidHttpReporter :: BaseUrl -> API.AsteroidTemplate -> L.AppL (Either String AsteroidId)
asteroidHttpReporter url a = do
  eAId <- L.callAPI url $ asteroid a
  pure $ case eAId of
    Left err -> Left $ show err
    Right r -> Right r


meteorTcpReporter :: TcpConn -> API.MeteorTemplate -> L.AppL (Either String MeteorId)
meteorTcpReporter _ m = do
  L.evalIO $ pure ()    -- send via tcp here
  L.logInfo "Meteor sent via TCP (dummy)."
  pure 0

asteroidTcpReporter :: TcpConn -> API.AsteroidTemplate -> L.AppL (Either String AsteroidId)
asteroidTcpReporter _ a = do
  L.evalIO $ pure ()    -- send via tcp here
  L.logInfo "Asteroid sent via TCP (dummy)."
  pure 0





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
runAstroClient =
  R.withAppRuntime (Just loggerCfg)
    $ \rt -> R.runAppL rt consoleApp
