{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import           Hydra.Prelude

import qualified Hydra.Domain  as D
import qualified Hydra.Runtime as R

import           Astro.Types
import           Astro.Catalogue

main :: IO ()
main = do

  loggerRt <- R.createVoidLoggerRuntime
  coreRt   <- R.createCoreRuntime loggerRt

  let cfg = AppConfig False 0

  void $ R.startApp coreRt $ astroCatalogue cfg
