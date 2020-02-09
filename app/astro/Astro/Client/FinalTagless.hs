{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}

module Astro.Client.FinalTagless
  ( consoleApp
  , httpAstroService
  , tcpAstroService
  ) where

import           Hydra.Prelude
import qualified Data.ByteString.Lazy  as BSL

import qualified Hydra.Domain          as D
import qualified Hydra.Language        as L

import qualified Astro.API             as API
import           Astro.Domain.Meteor   (MeteorId, Meteors)
import           Astro.Domain.Asteroid (AsteroidId)
import           Astro.Client.Common   (ReportChannel(..))
import qualified Astro.Client.Common   as C


class AstroService k m where
  reportMeteor     :: Proxy k -> API.MeteorTemplate   -> m (Either BSL.ByteString MeteorId)
  reportAsteroid   :: Proxy k -> API.AsteroidTemplate -> m (Either BSL.ByteString AsteroidId)

data HttpAstroService
data TcpAstroService

instance AstroService HttpAstroService L.AppL where
  reportMeteor   _ = C.reportMeteorHttp C.localhostAstro
  reportAsteroid _ = C.reportAsteroidHttp C.localhostAstro

instance AstroService TcpAstroService L.AppL where
  reportMeteor   _ = C.reportMeteorTcp C.tcpConn
  reportAsteroid _ = C.reportAsteroidTcp C.tcpConn

httpAstroService :: Proxy HttpAstroService
httpAstroService = Proxy

tcpAstroService :: Proxy TcpAstroService
tcpAstroService = Proxy

reportWith
  :: FromJSON obj
  => (obj -> L.AppL (Either BSL.ByteString res))
  -> (Either BSL.ByteString obj)
  -> L.AppL (Either BSL.ByteString ())
reportWith _        (Left err)  = pure $ Left err
reportWith reporter (Right obj) = reporter obj >> pure (Right ())

consoleApp
  :: forall k
   . AstroService k L.AppL
  => Proxy k
  -> L.AppL ()
consoleApp p = do
  line <- L.evalIO $ BSL.putStr "> " >> BSL.getContents

  let runners =
        [ reportWith (reportMeteor p)   $ C.tryParseCmd @(API.MeteorTemplate)   line
        , reportWith (reportAsteroid p) $ C.tryParseCmd @(API.AsteroidTemplate) line
        ]

  eResults <- sequence runners
  C.printResults eResults

  consoleApp p
