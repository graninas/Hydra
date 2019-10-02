{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE DeriveAnyClass         #-}


module Hydra.Core.Domain.SQLDB where

import           Hydra.Prelude

import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as LBS
import           System.FilePath ((</>))
import           Database.Beam.Sqlite (Sqlite)

import           Hydra.Core.Domain.DB

-- data SqlDBConfig be
--   = SQLiteConfig DBName
--   deriving (Show, Read, Ord, Eq, Generic, ToJSON, FromJSON)

data SQLiteConfig = SQLiteConfig DBName
  deriving (Show, Read, Ord, Eq, Generic, ToJSON, FromJSON)

mkSQLiteConfig :: DBName -> SQLiteConfig
mkSQLiteConfig = SQLiteConfig
