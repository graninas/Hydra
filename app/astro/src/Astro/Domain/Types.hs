{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Astro.Domain.Types where

import           Hydra.Prelude

import           Data.Time.Clock (UTCTime)

type DateTime = UTCTime

data Coords = Coords
  { azimuth  :: Int32
  , altitude :: Int32
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

type AstronomicalUnit = Double

data Orbital = Orbital
  { apoapsis              :: AstronomicalUnit
  , periapsis             :: AstronomicalUnit

  -- Enough fields for demo.

  -- , epoch                 :: UTCTime
  -- , semiMajorAxis         :: AstronomicalUnit
  -- , eccentrity            :: Double
  -- , inclination           :: Double
  -- , longitude             :: Double
  -- , argumentOfPeriapsis   :: Double
  -- , orbitalPeriod         :: Double
  -- , avgOrbitalSpeed       :: Double
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data Physical = Physical
  { meanDiameter    :: Double
  , rotationPeriod  :: Double


  -- Enough fields for demo.
  -- , albedo          :: Double
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- https://3d-asteroids.space/asteroids/79056-1132-T-3
