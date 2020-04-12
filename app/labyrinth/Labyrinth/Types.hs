{-# LANGUAGE DeriveAnyClass #-}

module Labyrinth.Types where

import qualified Data.Text                  as T
import qualified Data.Map                   as Map

import           Labyrinth.Prelude          as L
import           Labyrinth.Domain

type HasATreasure = Bool

data GameState = GameState
  { _labyrinth            :: StateVar Labyrinth
  , _playerPos            :: StateVar (Int, Int)
  , _playerInventory      :: Inventory
  , _playerIsAboutLeaving :: StateVar (Bool, HasATreasure)
  }


data AppException
  = NotImplemented Text
  | Finished Bool
  | InvalidOperation Text
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, Exception)
