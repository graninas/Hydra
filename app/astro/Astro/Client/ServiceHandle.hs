{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}

module Astro.Client.ServiceHandle
  ( consoleApp
  , makeServiceHandle
  ) where

import           Hydra.Prelude
import qualified Data.ByteString.Lazy  as BSL

import qualified Hydra.Domain          as D
import qualified Hydra.Language        as L

import qualified Astro.API             as API
import           Astro.Domain.Meteor   (MeteorId, Meteors)
import           Astro.Domain.Asteroid (AsteroidId)
import           Astro.Client.Common   (ReportChannel(..),
  tryParseCmd, reportMeteorTcp, reportAsteroidTcp, reportMeteorHttp,
  reportAsteroidHttp, localhostAstro, printResults, tcpConn)

data AstroServiceHandle = AstroServiceHandle
  { meteorReporter     :: API.MeteorTemplate   -> L.AppL (Either BSL.ByteString MeteorId)
  , asteroidReporter   :: API.AsteroidTemplate -> L.AppL (Either BSL.ByteString AsteroidId)
  }


reportWith
  :: FromJSON obj
  => (obj -> L.AppL (Either BSL.ByteString res))
  -> (Either BSL.ByteString obj)
  -> L.AppL (Either BSL.ByteString ())
reportWith _        (Left err)  = pure $ Left err
reportWith reporter (Right obj) = reporter obj >> pure (Right ())

makeServiceHandle :: ReportChannel -> AstroServiceHandle
makeServiceHandle TcpChannel  = AstroServiceHandle
  (reportMeteorTcp    tcpConn)
  (reportAsteroidTcp  tcpConn)
makeServiceHandle HttpChannel = AstroServiceHandle
  (reportMeteorHttp   localhostAstro)
  (reportAsteroidHttp localhostAstro)

consoleApp :: AstroServiceHandle -> L.AppL ()
consoleApp handle@(AstroServiceHandle{..}) = do
  line <- L.evalIO $ BSL.putStr "> " >> BSL.getContents

  let runners =
        [ reportWith meteorReporter   $ tryParseCmd @(API.MeteorTemplate)   line
        , reportWith asteroidReporter $ tryParseCmd @(API.AsteroidTemplate) line
        ]

  eResults <- sequence runners
  printResults eResults

  consoleApp handle
