module Types where

import           Hydra.Prelude

data Meteor = Meteor
  { size :: Int
  }
  deriving (Show, Eq, Ord)

data Region
  = NorthEast
  | NorthWest
  | SouthEast
  | SouthWest
  deriving (Show, Eq, Ord)
