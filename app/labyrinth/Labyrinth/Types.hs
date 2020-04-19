{-# LANGUAGE DeriveAnyClass #-}

module Labyrinth.Types where

import qualified Data.Text                  as T
import qualified Data.Map                   as Map

import           Labyrinth.Prelude          as L
import           Labyrinth.Domain

type HasTreasure = Bool

data Inventory = Inventory
  { _treasure :: StateVar Bool
  }

data GameState = GameState
  { _labyrinth            :: StateVar Labyrinth
  , _wormholes            :: Map Int (Int, Int)
  , _playerPos            :: StateVar Pos
  , _playerInventory      :: Inventory
  , _playerIsAboutLeaving :: StateVar (Maybe HasTreasure)
  , _gameFinished         :: StateVar Bool
  }

data AppException
  = NotImplemented Text
  | Finished Bool
  | InvalidOperation Text
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, Exception)
