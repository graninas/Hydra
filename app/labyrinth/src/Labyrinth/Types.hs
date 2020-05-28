{-# LANGUAGE DeriveAnyClass #-}

module Labyrinth.Types where


import Labyrinth.Prelude
import Labyrinth.Domain
import Labyrinth.KVDB.Model

data InventoryState = InventoryState
  { _treasureState :: StateVar Bool
  }

data GameState
  = GameStart
  | GameFinished
  | PlayerMove
  | PlayerIsAboutLeaving
  | PlayerIsAboutLossLeavingConfirmation
  deriving (Show, Eq)

data AppState = AppState
  { _labyrinth            :: StateVar Labyrinth
  , _labBounds            :: StateVar Bounds
  , _labRenderTemplate    :: StateVar LabRender
  , _labRenderVar         :: StateVar LabRender
  , _labWormholes         :: StateVar Wormholes
  , _playerPos            :: StateVar Pos
  , _playerHP             :: StateVar Int
  , _bearPos              :: StateVar Pos
  , _playerInventory      :: InventoryState
  , _gameState            :: StateVar GameState
  , _gameMessages         :: StateVar [String]
  , _kvdbConfig           :: KVDBConfig LabKVDB
  }

data AppException
  = NotImplemented String
  | NotSupported String
  | InvalidOperation String
  | GenerationError String
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, Exception)
