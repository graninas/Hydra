{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}

module Astro.Client.Common where

import           Hydra.Prelude
import qualified Data.ByteString.Lazy  as BSL
import           Data.Aeson            (decode)
import           Data.Either           (rights)
import           Servant
import           Servant.Client        (ClientM, BaseUrl(..), Scheme(..), client)

import qualified Hydra.Language        as L

import           Astro.Domain.Meteor   (MeteorId, Meteors)
import           Astro.Domain.Asteroid (AsteroidId)
import qualified Astro.Server          as Server
import qualified Astro.API             as API


data TcpConn = DummyTcpConn

data ReportChannel = TcpChannel | HttpChannel
  deriving (Show, Read)

data Approach
  = SH    -- ^ ServiceHandle
  | RT    -- ^ ReaderT
  | FM    -- ^ Free Monad
  | FT    -- ^ Final Tagless (mtl-style)
  | FT2   -- ^ Final Tagless 2 (mtl-style)
  | CEFM  -- ^ Church Encoded Free Monad
  | GADT  -- ^ GADT
  deriving (Show, Read)

meteors  :: Maybe Int -> Maybe Int -> ClientM Meteors
meteor   :: API.MeteorTemplate     -> ClientM MeteorId
asteroid :: API.AsteroidTemplate   -> ClientM AsteroidId
( meteors
 :<|> meteor
 :<|> asteroid
 :<|> setObjectTemplate
 :<|>
  ( getObject
  :<|> setOrbital
  :<|> setPhysical
  )
 ) = client Server.astroAPI

reportMeteorHttp :: BaseUrl -> API.MeteorTemplate -> L.AppL (Either BSL.ByteString MeteorId)
reportMeteorHttp url m = do
  eMId <- L.scenario $ L.callAPI url $ meteor m
  pure $ case eMId of
    Left err -> Left $ show err
    Right r  -> Right r

reportAsteroidHttp :: BaseUrl -> API.AsteroidTemplate -> L.AppL (Either BSL.ByteString AsteroidId)
reportAsteroidHttp url a = do
  eAId <- L.scenario $ L.callAPI url $ asteroid a
  pure $ case eAId of
    Left err -> Left $ show err
    Right r  -> Right r

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
  Nothing  -> Left "Decoding failed."
  Just obj -> Right obj

localhostAstro :: BaseUrl
localhostAstro = BaseUrl Http "localhost" 8081 ""

tcpConn :: TcpConn
tcpConn = DummyTcpConn

printResults :: [Either BSL.ByteString ()] -> L.AppL ()
printResults eResults = printResults' (rights eResults)
  where
    printResults' []   = L.evalIO $ BSL.putStrLn "Command is not recognized."
    printResults' [()] = pure ()
    printResults' _    = L.evalIO $ BSL.putStrLn "Multiple commands evaluated unexpectedly"
