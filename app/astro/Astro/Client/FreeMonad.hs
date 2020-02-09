{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}

module Astro.Client.FreeMonad
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
import           Astro.Client.Common   (TcpConn(..), ReportChannel(..))
import qualified Astro.Client.Common   as C


data AstroServiceF a where
  ReportMeteor   :: API.MeteorTemplate   -> (Either BSL.ByteString MeteorId   -> next) -> AstroServiceF next
  ReportAsteroid :: API.AsteroidTemplate -> (Either BSL.ByteString AsteroidId -> next) -> AstroServiceF next

instance Functor AstroServiceF where
  fmap f (ReportMeteor m next) = ReportMeteor m (f . next)
  fmap f (ReportAsteroid a next) = ReportAsteroid a (f . next)

type AstroService a = Free AstroServiceF a

-- type AstroServiceRunner a = AstroServiceF a -> L.AppL a

reportMeteor :: API.MeteorTemplate -> AstroService (Either BSL.ByteString MeteorId)
reportMeteor m = liftF $ ReportMeteor m id

reportAsteroid :: API.AsteroidTemplate -> AstroService (Either BSL.ByteString AsteroidId)
reportAsteroid a = liftF $ ReportAsteroid a id

asTcpAstroService :: AstroServiceF a -> L.AppL a
asTcpAstroService (ReportMeteor m next)   = next <$> C.reportMeteorTcp C.tcpConn m
asTcpAstroService (ReportAsteroid a next) = next <$> C.reportAsteroidTcp C.tcpConn a

asHttpAstroService :: AstroServiceF a -> L.AppL a
asHttpAstroService (ReportMeteor m next)   = next <$> C.reportMeteorHttp C.localhostAstro m
asHttpAstroService (ReportAsteroid a next) = next <$> C.reportAsteroidHttp C.localhostAstro a

runAstroService :: (forall x. AstroServiceF x -> L.AppL x) -> AstroService a -> L.AppL a
runAstroService runner act = foldFree runner act

reportWith
  :: FromJSON obj
  => (forall x. AstroServiceF x -> L.AppL x)
  -> (obj -> AstroService a)
  -> (Either BSL.ByteString obj)
  -> L.AppL (Either BSL.ByteString ())
reportWith runner _        (Left err)  = pure $ Left err
reportWith runner reporter (Right obj) = (runAstroService runner $ reporter obj) >> pure (Right ())

getAstroServiceRunner :: ReportChannel -> (AstroServiceF a -> L.AppL a)
getAstroServiceRunner TcpChannel  = asTcpAstroService
getAstroServiceRunner HttpChannel = asHttpAstroService

consoleApp :: (forall x. AstroServiceF x -> L.AppL x) -> L.AppL ()
consoleApp runner = do
  line <- L.evalIO $ BSL.putStr "> " >> BSL.getContents

  let runners =
        [ reportWith runner reportMeteor   $ C.tryParseCmd @(API.MeteorTemplate)   line
        , reportWith runner reportAsteroid $ C.tryParseCmd @(API.AsteroidTemplate) line
        ]

  eResults <- sequence runners
  C.printResults eResults

  consoleApp runner
