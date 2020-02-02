{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}

module Astro.Client
  ( runAstroClient
  ) where

import           Hydra.Prelude

import qualified Hydra.Domain  as D
import qualified Hydra.Runtime as R
import qualified Hydra.Interpreters as R
import qualified Hydra.Language     as L

import           Astro.Domain.Meteor
import           Astro.Types


runAstroClient :: IO ()
runAstroClient = pure ()
