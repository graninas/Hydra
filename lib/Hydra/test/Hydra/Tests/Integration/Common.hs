{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PackageImports         #-}
{-# LANGUAGE TemplateHaskell        #-}

module Hydra.Tests.Integration.Common where

import           Hydra.Prelude

import qualified Hydra.Domain                      as D
import qualified Hydra.Language                    as L
import           Database.Beam
import qualified Database.Beam as B
import qualified Database.Beam.Query as B

import           Hydra.TestData
import           Hydra.TestData.Types.Meteor
import qualified Hydra.TestData.Types.SqlDB.CatalogueDB as CatDB

getMeteorsWithMass size
  = B.select
  $ B.filter_ (\meteor -> CatDB._size meteor ==. B.val_ size)
  $ B.all_ (CatDB._meteors CatDB.catalogueDB)



convertMeteor :: CatDB.DBMeteor -> Meteor
convertMeteor m = Meteor
  { _id        = CatDB._id m
  , _size      = CatDB._size m
  , _mass      = CatDB._mass m
  , _coords    = Coords (CatDB._azimuth m) (CatDB._altitude m)
  , _timestamp = CatDB._timestamp m
  }
