{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Astro.Domain.Meteor where

import           Hydra.Prelude

import           Data.Time.Clock (UTCTime)

type DateTime = UTCTime

data Coords = Coords
  { azimuth  :: Int32
  , altitude :: Int32
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

type MeteorId = Int32

data Meteor = Meteor
  { meteorId    :: MeteorId
  , size        :: Int32
  , mass        :: Int32
  , coords      :: Coords
  , timestamp   :: DateTime
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

newtype Meteors = Meteors [Meteor]
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
