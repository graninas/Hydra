{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}

module Astro.Client.ReaderT
  ( consoleApp
  , makeAppEnv
  ) where

import           Hydra.Prelude
import qualified Data.ByteString.Lazy  as BSL
import           Servant.Client        (BaseUrl(..), Scheme(..))

import qualified Hydra.Domain          as D
import qualified Hydra.Language        as L

import qualified Astro.API             as API
import           Astro.Domain.Meteor   (MeteorId, Meteors)
import           Astro.Domain.Asteroid (AsteroidId)
import           Astro.Client.Common   (ReportChannel(..),
  tryParseCmd, reportMeteorTcp, reportAsteroidTcp, reportMeteorHttp,
  reportAsteroidHttp, localhostAstro, printResults, tcpConn)

data AppEnv = AppEnv
  { meteorReporter     :: API.MeteorTemplate   -> L.AppL (Either BSL.ByteString MeteorId)
  , asteroidReporter   :: API.AsteroidTemplate -> L.AppL (Either BSL.ByteString AsteroidId)
  }

type AppRT a = ReaderT AppEnv L.AppL a

reportWith
  :: FromJSON obj
  => (obj -> L.AppL (Either BSL.ByteString res))
  -> (Either BSL.ByteString obj)
  -> AppRT (Either BSL.ByteString ())
reportWith _        (Left err)  = pure $ Left err
reportWith reporter (Right obj) = lift (reporter obj) >> pure (Right ())

makeAppEnv :: ReportChannel -> AppEnv
makeAppEnv TcpChannel  = AppEnv
  (reportMeteorTcp    tcpConn)
  (reportAsteroidTcp  tcpConn)
makeAppEnv HttpChannel = AppEnv
  (reportMeteorHttp   localhostAstro)
  (reportAsteroidHttp localhostAstro)

consoleApp :: AppRT ()
consoleApp = do
  AppEnv {..} <- ask
  line <- lift $ L.evalIO $ BSL.putStr "> " >> BSL.getContents

  let runners =
        [ reportWith meteorReporter   $ tryParseCmd @(API.MeteorTemplate)   line
        , reportWith asteroidReporter $ tryParseCmd @(API.AsteroidTemplate) line
        ]

  eResults <- sequence runners
  lift $ printResults eResults

  consoleApp
