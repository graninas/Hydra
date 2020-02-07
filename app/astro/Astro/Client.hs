{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}

module Astro.Client
  ( ReportChannel (..)
  , runAstroClient
  ) where

import           Hydra.Prelude
import qualified Data.ByteString.Lazy as BSL
import           Data.Aeson (decode)
import           Data.Either (rights)
import           Servant
import           Servant.Client (ClientM, ClientError, BaseUrl(..), Scheme(..), client)

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


data ReportChannel = TcpChannel | HttpChannel

data AstroServerHandle = AstroServerHandle
  { meteorReporter     :: API.MeteorTemplate   -> L.AppL (Either BSL.ByteString MeteorId)
  , asteroidReporter   :: API.AsteroidTemplate -> L.AppL (Either BSL.ByteString AsteroidId)
  }


meteors :: Maybe Int -> Maybe Int -> ClientM Meteors
meteor :: API.MeteorTemplate -> ClientM MeteorId
asteroid :: API.AsteroidTemplate -> ClientM AsteroidId
(meteors :<|> meteor :<|> asteroid) = client Server.astroAPI

reportMeteorHttp :: BaseUrl -> API.MeteorTemplate -> L.AppL (Either BSL.ByteString MeteorId)
reportMeteorHttp url m = do
  eMId <- L.scenario $ L.callAPI url $ meteor m
  pure $ case eMId of
    Left err -> Left $ show err
    Right r -> Right r

reportAsteroidHttp :: BaseUrl -> API.AsteroidTemplate -> L.AppL (Either  BSL.ByteString AsteroidId)
reportAsteroidHttp url a = do
  eAId <- L.scenario $ L.callAPI url $ asteroid a
  pure $ case eAId of
    Left err -> Left $ show err
    Right r -> Right r

reportMeteorTcp :: TcpConn -> API.MeteorTemplate -> L.AppL (Either BSL.ByteString MeteorId)
reportMeteorTcp _ m = do
  L.evalIO $ pure ()    -- send via tcp here
  L.logInfo "Meteor sent via TCP (dummy)."
  pure $ Right 0

reportAsteroidTcp :: TcpConn -> API.AsteroidTemplate -> L.AppL (Either BSL.ByteString AsteroidId)
reportAsteroidTcp _ a = do
  L.evalIO $ pure ()    -- send via tcp here
  L.logInfo "Asteroid sent via TCP (dummy)."
  pure $ Right 0


tryParseCmd
  :: FromJSON obj
  => BSL.ByteString
  -> Either BSL.ByteString obj
tryParseCmd str = case decode str of
  Nothing -> Left "Decoding failed."
  Just obj -> Right obj

reportWith
  :: FromJSON obj
  => (obj -> L.AppL (Either BSL.ByteString res))
  -> (Either BSL.ByteString obj)
  -> L.AppL (Either BSL.ByteString ())
reportWith reporter (Left err) = pure $ Left err
reportWith reporter (Right obj) = reporter obj >> pure (Right ())

consoleApp :: AstroServerHandle -> L.AppL ()
consoleApp handle@(AstroServerHandle{..}) = do
  line <- L.evalIO $ BSL.putStr "> " >> BSL.getContents

  let runners =
        [ reportWith meteorReporter   $ tryParseCmd @(API.MeteorTemplate)   line
        , reportWith asteroidReporter $ tryParseCmd @(API.AsteroidTemplate) line
        ]

  eResults <- sequence runners
  case rights eResults of
    [] -> L.evalIO $ BSL.putStrLn "Command is not recognized."
    [()] -> pure ()
    (_) -> L.evalIO $ BSL.putStrLn "Multiple commands evaluated unexpectedly"

  consoleApp handle

makeReporters :: ReportChannel -> AstroServerHandle
makeReporters TcpChannel  = AstroServerHandle
  (reportMeteorTcp    DummyTcpConn)
  (reportAsteroidTcp  DummyTcpConn)
makeReporters HttpChannel = AstroServerHandle
  (reportMeteorHttp   localhostAstro)
  (reportAsteroidHttp localhostAstro)
  where
    localhostAstro = BaseUrl Http "localhost" 8081 ""

runAstroClient :: ReportChannel -> IO ()
runAstroClient ch =
  R.withAppRuntime (Just loggerCfg)
    $ \rt -> R.runAppL rt $ consoleApp $ makeReporters ch
