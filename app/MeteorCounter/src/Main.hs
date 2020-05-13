{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad
import           Hydra.Prelude

import qualified Free          as Free
import           Types
import qualified FTL           as FTL
import qualified Church        as Church
import qualified Hydra.Domain  as D
import qualified Hydra.Runtime as R
import qualified Hydra.Framework.RLens as RLens

data Method = FT | FreeM | ChurchM
  deriving (Show, Read, Eq, Ord)

data Config = Config
  { useLog    :: Bool
  , method    :: Method
  , appConfig :: AppConfig
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

  let mbLoggerCfg = if useLog cfg then Just loggerCfg else Nothing
  
  R.withAppRuntime mbLoggerCfg $ \appRt -> do
    when (method cfg == FT)
      $ FTL.scenario (appRt ^. RLens.coreRuntime)
      $ appConfig cfg

    when (method cfg == FreeM)
      $ Free.scenario appRt
      $ appConfig cfg

    when (method cfg == ChurchM)
      $ Church.scenario appRt
      $ appConfig cfg
