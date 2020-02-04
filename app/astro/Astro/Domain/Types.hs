{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Astro.Domain.Types where

import           Hydra.Prelude

import qualified Data.Map      as Map
import qualified Data.Set      as Set
import           Data.Time.Clock (UTCTime)

type DateTime = UTCTime

data Coords = Coords
  { azimuth  :: Int
  , altitude :: Int
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

type AstronomicalUnit = Double

data Orbital = Orbital
  { epoch                 :: UTCTime
  , apoapsis              :: AstronomicalUnit
  , periapsis             :: AstronomicalUnit
  , semiMajorAxis         :: AstronomicalUnit
  , Eccentrity            :: Double
  , Inclination           :: Double
  , Longitude             :: Double
  , ArgumentOfPeriapsis   :: Double
  , OrbitalPeriod         :: Double
  , AvgOrbitalSpeed       :: Double
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data Physical = Physical
  { meanDiameter    :: Double
  , rotationPeriod  :: Double
  , albedo          :: Double
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- https://3d-asteroids.space/asteroids/79056-1132-T-3
