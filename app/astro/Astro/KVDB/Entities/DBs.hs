module Astro.KVDB.Entities.DBs where

import qualified Hydra.Domain          as D




data CatalogueDB


instance D.DB CatalogueDB where
  getDBName = "catalogue.rdb"
