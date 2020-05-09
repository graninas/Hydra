{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}

module Astro.Client.FinalTagless
  ( consoleApp
  , HttpAstroService
  , TcpAstroService
  ) where

import           Hydra.Prelude
import qualified Data.ByteString.Lazy  as BSL

import qualified Hydra.Language        as L

import qualified Astro.API             as API
import           Astro.Domain.Meteor   (MeteorId)
import           Astro.Domain.Asteroid (AsteroidId)
import qualified Astro.Client.Common   as C


class AstroService k where
  reportMeteor     :: API.MeteorTemplate   -> L.AppL (Either BSL.ByteString MeteorId)
  reportAsteroid   :: API.AsteroidTemplate -> L.AppL (Either BSL.ByteString AsteroidId)

data HttpAstroService
data TcpAstroService

instance AstroService HttpAstroService where
  reportMeteor   = C.reportMeteorHttp C.localhostAstro
  reportAsteroid = C.reportAsteroidHttp C.localhostAstro

instance AstroService TcpAstroService where
  reportMeteor   = C.reportMeteorTcp C.tcpConn
  reportAsteroid = C.reportAsteroidTcp C.tcpConn

reportWith
  :: FromJSON obj
  => (obj -> L.AppL (Either BSL.ByteString res))
  -> (Either BSL.ByteString obj)
  -> L.AppL (Either BSL.ByteString ())
reportWith _        (Left err)  = pure $ Left err
reportWith reporter (Right obj) = reporter obj >> pure (Right ())

consoleApp
  :: forall k
   . AstroService k
  => L.AppL ()
consoleApp = do
  line <- L.evalIO $ BSL.putStr "> " >> BSL.getContents

  let runners =
        [ reportWith (reportMeteor @k)   $ C.tryParseCmd line
        , reportWith (reportAsteroid @k) $ C.tryParseCmd line
        ]

  eResults <- sequence runners
  C.printResults eResults

  consoleApp @k
