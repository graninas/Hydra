{-# LANGUAGE DeriveAnyClass #-}

module Hydra.TestData.Types.Meteor where

import           Hydra.Prelude

import qualified Data.Map      as Map
import qualified Data.Set      as Set
import           Data.Time.Clock (UTCTime)

type DateTime = UTCTime

data Coords = Coords
  { _azimuth  :: Int32
  , _altitude :: Int32
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data Meteor' k = Meteor
  { _id          :: k
  , _size        :: Int32
  , _mass        :: Int32
  , _coords      :: Coords
  , _timestamp   :: DateTime
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

type RawMeteor = Meteor' ()
type Meteor    = Meteor' Int32
