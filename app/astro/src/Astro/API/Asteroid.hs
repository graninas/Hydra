{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Astro.API.Asteroid where

import           Hydra.Prelude

import           Astro.Domain.Types

data AsteroidTemplate = AsteroidTemplate
  { name        :: Maybe Text
  , orbital     :: Orbital
  , physical    :: Physical
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
