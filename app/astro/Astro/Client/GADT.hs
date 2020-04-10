{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}

module Astro.Client.GADT
  ( consoleApp
  , getAstroServiceRunner
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


data AstroService a where
  ReportMeteor   :: API.MeteorTemplate   -> AstroService (Either BSL.ByteString MeteorId)
  ReportAsteroid :: API.AsteroidTemplate -> AstroService (Either BSL.ByteString AsteroidId)

asTcpAstroService :: AstroService a -> L.AppL a
asTcpAstroService (ReportMeteor m)   = C.reportMeteorTcp C.tcpConn m
asTcpAstroService (ReportAsteroid a) = C.reportAsteroidTcp C.tcpConn a

asHttpAstroService :: AstroService a -> L.AppL a
asHttpAstroService (ReportMeteor m)   = C.reportMeteorHttp C.localhostAstro m
asHttpAstroService (ReportAsteroid a) = C.reportAsteroidHttp C.localhostAstro a

getAstroServiceRunner :: ReportChannel -> (AstroService a -> L.AppL a)
getAstroServiceRunner TcpChannel  = asTcpAstroService
getAstroServiceRunner HttpChannel = asHttpAstroService

reportWith
  :: FromJSON obj
  => (forall x. AstroService x -> L.AppL x)
  -> (obj -> AstroService a)
  -> (Either BSL.ByteString obj)
  -> L.AppL (Either BSL.ByteString ())
reportWith runner _        (Left err)  = pure $ Left err
reportWith runner reporter (Right obj) = (runner $ reporter obj) >> pure (Right ())

consoleApp :: (forall x. AstroService x -> L.AppL x) -> L.AppL ()
consoleApp runner = do
  line <- L.evalIO $ BSL.putStr "> " >> BSL.getContents

  let runners =
        [ reportWith runner ReportMeteor   $ C.tryParseCmd line
        , reportWith runner ReportAsteroid $ C.tryParseCmd line
        ]

  eResults <- sequence runners
  C.printResults eResults

  consoleApp runner
