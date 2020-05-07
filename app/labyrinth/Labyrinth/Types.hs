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

type LabRender = Map Pos String
type Wormholes = Map Int Pos

data GameState = GameState
  { _labyrinth            :: StateVar Labyrinth
  , _labyrinthSize        :: StateVar Bounds
  , _labRenderTemplate    :: LabRender
  , _labRenderVar         :: StateVar LabRender
  , _wormholes            :: Wormholes
  , _playerPos            :: StateVar Pos
  , _playerInventory      :: Inventory
  , _playerIsAboutLeaving :: StateVar (Maybe HasTreasure)
  , _gameFinished         :: SignalVar    -- StateVar Bool
  , _moveMessages         :: StateVar [String]
  }

data AppException
  = NotImplemented String
  | Finished Bool
  | InvalidOperation String
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, Exception)
