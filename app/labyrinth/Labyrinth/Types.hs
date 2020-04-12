{-# LANGUAGE DeriveAnyClass #-}

module Labyrinth.Types where

import qualified Data.Text                  as T
import qualified Data.Map                   as Map

import           Labyrinth.Prelude          as L
import           Labyrinth.Domain

data GameState = GameState
  { _labyrinth :: Labyrinth
  }


data AppException
  = NotImplemented Text
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, Exception)
