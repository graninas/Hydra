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

mkLoggerCfg :: D.LoggerConfig
mkLoggerCfg = D.LoggerConfig
  { D._format       = "$prio $loggername: $msg"
  , D._level        = D.Debug
  , D._logFilePath  = ""
  , D._logToConsole = True
  , D._logToFile    = False
  }

main :: IO ()
main = do
  let loggerCfg = mkLoggerCfg
  let cfg = AppConfig False 0
  let appF appRt = void $ R.startApp appRt $ astroCatalogue cfg
  R.withAppRuntime (Just loggerCfg) appF
