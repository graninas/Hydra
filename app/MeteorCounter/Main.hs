{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import           Hydra.Prelude

import qualified Free          as Free
-- import qualified FTL           as FTL
import qualified Church        as Church
import qualified Hydra.Domain  as D
import qualified Hydra.Runtime as R

data Method = FT | FreeM | ChurchM
  deriving (Show, Read, Eq, Ord)

data Config = Config
  { useLog     :: Bool
  , method     :: Method
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
  cfgStr <- readFile "meteor_counter.cfg"
  let cfg :: Config = read $ toString cfgStr

  putStrLn @String $ "Method: " <> show (method cfg)

  loggerRt <- if useLog cfg
    then R.createLoggerRuntime loggerCfg
    else R.createVoidLoggerRuntime
  coreRt <- R.createCoreRuntime loggerRt

  when (method cfg == FT) $
    -- FTL.scenario1 ops coreRt
    putStrLn @String "FT is not supported for this scenario."

  when (method cfg == FreeM) $
    Free.scenario coreRt

  when (method cfg == ChurchM) $
    Church.scenario coreRt
