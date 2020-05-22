{-# LANGUAGE DeriveAnyClass #-}

module Labyrinth.Types where


import           Labyrinth.Prelude          as L
import           Labyrinth.Domain

type HasTreasure = Bool

data Inventory = Inventory
  { _treasure :: StateVar Bool
  }

data GameState
  = GameStart
  | GameFinished
  | PlayerMove
  | PlayerIsAboutLeaving HasTreasure
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
  , _playerInventory      :: Inventory
  , _gameState            :: StateVar GameState
  , _gameMessages         :: StateVar [String]
  }

data AppException
  = NotImplemented String
  | NotSupported String
  | InvalidOperation String
  | GenerationError String
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, Exception)
