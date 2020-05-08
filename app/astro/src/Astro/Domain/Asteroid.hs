{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Astro.Domain.Asteroid where

import           Hydra.Prelude

import qualified Data.Map      as Map
import qualified Data.Set      as Set
import           Data.Time.Clock (UTCTime)

import           Astro.Domain.Types

type AsteroidId  = Int

data Asteroid = Asteroid
  { asteroidId  :: AsteroidId
  , name        :: Maybe Text
  , orbital     :: Orbital
  , physical    :: Physical
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)


type Asteroids = [Asteroid]
