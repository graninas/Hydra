{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}

module Astro.Client.Common where

import           Hydra.Prelude
import qualified Data.ByteString.Lazy  as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.Aeson            (decode)
import           Servant
import           Servant.Client        (ClientM, BaseUrl(..), Scheme(..), client)

import qualified Hydra.Language        as L

import           Astro.Domain.Meteor   (MeteorId, Meteors)
import           Astro.Domain.Asteroid (AsteroidId)
import qualified Astro.API             as API
import           Astro.Domain.AstroObject (AstroObject)
import           Astro.Domain.Types


data TcpConn = DummyTcpConn

data ReportChannel
  = TcpChannel
  | HttpChannel
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data DIApproach
  = SH    -- ^ ServiceHandle
  | RT    -- ^ ReaderT
  | FM    -- ^ Free Monad
  | FT    -- ^ Final Tagless (mtl-style)
  | FT2   -- ^ Final Tagless 2 (mtl-style)
  | CEFM  -- ^ Church Encoded Free Monad
  | GADT  -- ^ GADT
  deriving (Show, Read, Bounded, Eq, Ord, Enum)

setPhysical :: Int32 -> Physical -> ClientM Int32
setOrbital :: Int32 -> Orbital -> ClientM Int32
setObjectTemplate :: API.AstroObjectTemplate -> ClientM Int32
getObject :: Int32 -> ClientM (Maybe AstroObject)

meteors  :: Maybe Int32 -> Maybe Int32 -> ClientM Meteors
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
 ) = client API.astroAPI

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
reportMeteorTcp _ _ = do
  L.evalIO $ pure ()    -- send via tcp here
  L.logInfo "Meteor sent via TCP (dummy)."
  pure $ Right 0

reportAsteroidTcp :: TcpConn -> API.AsteroidTemplate -> L.AppL (Either BSL.ByteString AsteroidId)
reportAsteroidTcp _ _ = do
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
    printResults' []   = L.evalIO $ BSL8.putStrLn "Command is not recognized."
    printResults' [()] = pure ()
    printResults' _    = L.evalIO $ BSL8.putStrLn "Multiple commands evaluated unexpectedly"
