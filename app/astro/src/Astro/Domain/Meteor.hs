{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Astro.Domain.Meteor where

import           Hydra.Prelude

import           Data.Time.Clock (UTCTime)

type DateTime = UTCTime

data Coords = Coords
  { azimuth  :: Int
  , altitude :: Int
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

type MeteorId  = Int

data Meteor = Meteor
  { meteorId    :: MeteorId
  , size        :: Int
  , mass        :: Int
  , coords      :: Coords
  , timestamp   :: DateTime
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

newtype Meteors = Meteors [Meteor]
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
