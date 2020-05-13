{-# LANGUAGE DeriveAnyClass #-}

module Labyrinth.Types where


import           Labyrinth.Prelude          as L
import           Labyrinth.Domain

type HasTreasure = Bool

data Inventory = Inventory
  { _treasure :: StateVar Bool
  }

type LabRender = Map Pos String
type Wormholes = Map Int Pos

data GameState
  = PlayerMove
  | PlayerIsAboutLeaving HasTreasure
  | PlayerIsAboutLossLeavingConfirmation
  | GameFinished
  deriving (Show, Eq)

data AppState = AppState
  { _labyrinth            :: StateVar Labyrinth
  , _labyrinthSize        :: StateVar Bounds
  , _labRenderTemplate    :: LabRender
  , _labRenderVar         :: StateVar LabRender
  , _wormholes            :: Wormholes
  , _playerPos            :: StateVar Pos
  , _playerInventory      :: Inventory
  , _gameState            :: StateVar GameState
  , _moveMessages         :: StateVar [String]
  }

data AppException
  = NotImplemented String
  | Finished Bool
  | InvalidOperation String
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, Exception)
