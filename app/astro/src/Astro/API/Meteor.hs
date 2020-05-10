{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Astro.API.Meteor where

import           Hydra.Prelude

data MeteorTemplate = MeteorTemplate
  { size        :: Int
  , mass        :: Int
  , azimuth     :: Int
  , altitude    :: Int
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
