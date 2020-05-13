{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Astro.API.AstroObject where

import           Hydra.Prelude

data AstroObjectTemplate = AstroObjectTemplate
  { name        :: Maybe Text
  , objectClass :: Text
  , code        :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
