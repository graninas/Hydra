{-# LANGUAGE DeriveAnyClass #-}

module Astro.Domain.Meteor where

import           Hydra.Prelude

import qualified Data.Map      as Map
import qualified Data.Set      as Set
import           Data.Time.Clock (UTCTime)

type DateTime = UTCTime

data Coords = Coords
  { _azimuth  :: Int
  , _altitude :: Int
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data Meteor' k = Meteor'
  { _id          :: k
  , _size        :: Int
  , _mass        :: Int
  , _coords      :: Coords
  , _timestamp   :: DateTime
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

type RawMeteor = Meteor' ()
type Meteor    = Meteor' Int

-- type Meteors = D.StateVar (Set.Set Meteor)
