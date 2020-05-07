{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}

module Main where

import           Control.Monad
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import           Hydra.Prelude

import qualified PerfFree          as Free
import qualified PerfFTL           as FTL
import qualified PerfChurch        as Church
import qualified Hydra.Domain  as D
import qualified Hydra.Runtime as R
import qualified Hydra.Framework.RLens as RLens

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
  cfgStr <- readFile "perf_test_app.cfg"
  let cfg :: Config = read $ toString cfgStr

  putStrLn @String $ "Method: " <> show (method cfg) <> ", iterations: " <> show (iterations cfg)

  let ops = iterations cfg

  let mbLoggerCfg = if useLog cfg then Just loggerCfg else Nothing

  R.withAppRuntime mbLoggerCfg $ \appRt -> do
    when (method cfg == FT) $ do
      when (scenario1 cfg) $ FTL.scenario1 ops $ appRt ^. RLens.coreRuntime
      when (scenario2 cfg) $ FTL.scenario2 ops $ appRt ^. RLens.coreRuntime
      when (scenario3 cfg) $ FTL.scenario3 ops $ appRt ^. RLens.coreRuntime

    when (method cfg == FreeM) $ do
      when (scenario1 cfg) $ Free.scenario1 ops appRt
      when (scenario2 cfg) $ Free.scenario2 ops appRt
      when (scenario3 cfg) $ Free.scenario3 ops appRt

    when (method cfg == ChurchM) $ do
      when (scenario1 cfg) $ Church.scenario1 ops appRt
      when (scenario2 cfg) $ Church.scenario2 ops appRt
      when (scenario3 cfg) $ Church.scenario3 ops appRt
