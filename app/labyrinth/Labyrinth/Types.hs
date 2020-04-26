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

type LabRender = Map Pos Text

data GameState = GameState
  { _labyrinth            :: StateVar Labyrinth
  , _labyrinthSize        :: StateVar Pos
  , _labRenderVar         :: StateVar LabRender
  , _wormholes            :: Map Int Pos
  , _playerPos            :: StateVar Pos
  , _playerInventory      :: Inventory
  , _playerIsAboutLeaving :: StateVar (Maybe HasTreasure)
  , _gameFinished         :: SignalVar    -- StateVar Bool
  }

data AppException
  = NotImplemented Text
  | Finished Bool
  | InvalidOperation Text
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, Exception)
