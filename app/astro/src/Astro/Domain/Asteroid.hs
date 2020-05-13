{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Astro.Domain.Asteroid where

import           Hydra.Prelude

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
