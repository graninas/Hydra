{-# LANGUAGE DeriveAnyClass #-}

module Astro.Types where

import qualified Data.Map      as Map
import qualified Data.Set      as Set
import           Data.Time.Clock (UTCTime)

import           Hydra.Prelude
import qualified Hydra.Domain  as D
import qualified Hydra.Runtime as R

import qualified Astro.KVDB.AstroDB as KVDB


data AppConfig = AppConfig
  deriving (Show, Read, Eq, Ord)

data AppState = AppState
  { _astroKVDB    :: D.DBHandle KVDB.AstroDB
  , _totalMeteors :: D.StateVar Int
  , _config       :: AppConfig
  }


data AppException
  = ConnectionFailedException Text
  | OperationFailedException Text
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, Exception)
