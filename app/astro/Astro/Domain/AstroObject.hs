{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Astro.Domain.AstroObject where

import           Hydra.Prelude

import           Astro.Domain.Types

type AstroObjectId  = Int


data AstroObject = AstroObject
  { astroObjectId :: AstroObjectId
  , name          :: Maybe Text
  , objectCass    :: Text
  , code          :: Text
  , orbital       :: Orbital
  , physical      :: Physical
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
