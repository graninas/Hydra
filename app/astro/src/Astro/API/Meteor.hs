{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Astro.API.Meteor where

import           Hydra.Prelude

data MeteorTemplate = MeteorTemplate
  { size        :: Int32
  , mass        :: Int32
  , azimuth     :: Int32
  , altitude    :: Int32
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
