{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module Hydra.Frameork.App.FTL where

import           Hydra.Prelude

import           Hydra.Core.FTL as L


type AppL m =
  ( L.LangL m


  )
