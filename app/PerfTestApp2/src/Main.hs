{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import           Hydra.Prelude

import qualified Perf2Free     as Free
import qualified Perf2FTL      as FTL
import qualified Perf2Church   as Church
import qualified IO            as IO
import qualified Hydra.Domain  as D
import qualified Hydra.Runtime as R
import qualified Hydra.Framework.RLens as RLens

data Method = FT | FreeM | ChurchM | IO
  deriving (Show, Read, Eq, Ord)

data Config = Config
  { method     :: Method
  , iterations :: Int
  }
  deriving (Show, Read, Eq, Ord)

loggerCfg :: D.LoggerConfig
loggerCfg = D.LoggerConfig
  { D._format       = "$prio $loggername: $msg"
  , D._level        = D.Debug
  , D._logFilePath  = ""
  , D._logToConsole = True
  , D._logToFile    = False
  }

main :: IO ()
main = do
  cfgStr <- readFile "perf_test_app2.cfg"
  let cfg :: Config = read $ toString cfgStr

  putStrLn @String $ "Method: " <> show (method cfg) <> ", iterations: " <> show (iterations cfg)

  let ops = iterations cfg

  R.withAppRuntime Nothing $ \appRt -> do
    when (method cfg == FT)
      $ FTL.scenario ops $ appRt ^. RLens.coreRuntime

    when (method cfg == FreeM)
      $ Free.scenario ops appRt

    when (method cfg == ChurchM)
      $ Church.scenario ops appRt

    when (method cfg == IO)
      $ IO.scenario ops
