{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import           Hydra.Prelude

import qualified Free          as Free
import qualified FTL           as FTL
import qualified Church        as Church
import qualified Hydra.Domain  as D
import qualified Hydra.Runtime as R

data Method = FT | FreeM | ChurchM
  deriving (Show, Read, Eq, Ord)

data Config = Config
  { useLog     :: Bool
  , method     :: Method
  , iterations :: Int
  , scenario1  :: Bool
  , scenario2  :: Bool
  , scenario3  :: Bool
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
  cfgStr <- readFile "cfg"
  let cfg :: Config = read $ toString cfgStr

  putStrLn @String $ "Method: " <> show (method cfg) <> ", iterations: " <> show (iterations cfg)

  loggerRt <- if useLog cfg
    then R.createLoggerRuntime loggerCfg
    else R.createVoidLoggerRuntime
  coreRt <- R.createCoreRuntime loggerRt

  let ops = iterations cfg

  when (method cfg == FT) $ do
    when (scenario1 cfg) $ FTL.scenario1 ops coreRt
    when (scenario2 cfg) $ FTL.scenario2 ops coreRt
    when (scenario3 cfg) $ FTL.scenario3 ops coreRt

  when (method cfg == FreeM) $ do
    when (scenario1 cfg) $ Free.scenario1 ops coreRt
    when (scenario2 cfg) $ Free.scenario2 ops coreRt
    when (scenario3 cfg) $ Free.scenario3 ops coreRt

  when (method cfg == ChurchM) $ do
    when (scenario1 cfg) $ Church.scenario1 ops coreRt
    when (scenario2 cfg) $ Church.scenario2 ops coreRt
    when (scenario3 cfg) $ Church.scenario3 ops coreRt
