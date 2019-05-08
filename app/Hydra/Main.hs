{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import qualified Data.Map             as Map
import qualified Data.Set             as Set

import qualified Hydra.Domain         as D
import qualified Hydra.FTL            as L
import           Hydra.Prelude
import qualified Hydra.Runtime        as R

import Hydra.FTLI ()

data Meteor = Meteor
  { size :: Int
  }
  deriving (Show, Eq, Ord)

data Region
  = NorthEast
  | NorthWest
  | SouthEast
  | SouthWest
  deriving (Show, Eq, Ord)


getRandomMeteor :: L.RandomL m => m Meteor
getRandomMeteor = Meteor <$> L.getRandomInt (1, 100)

getRandomRegion :: L.RandomL m => m Region
getRandomRegion = toRegion <$> L.getRandomInt (1, 4)
  where
    toRegion 1 = NorthWest
    toRegion 2 = NorthEast
    toRegion 3 = SouthWest
    toRegion _ = SouthEast

createMeteor :: L.LangL m => m (Meteor, Region)
createMeteor = do
  meteor <- getRandomMeteor
  region <- getRandomRegion
  pure (meteor, region)

meteorStorm :: L.LangL m => m ()
meteorStorm = do
  (meteor, region) <- createMeteor
  L.logInfo $ "[MS] " <> " a new meteor appeared at " <> show region <> ": " <> show meteor

meteorStormRec :: L.LangL m => Int -> m ()
meteorStormRec 0 = pure ()
meteorStormRec n = do
  meteorStorm
  meteorStormRec (n - 1)

meteorStormRec2 :: L.LangL m => Int -> m ()
meteorStormRec2 0 = pure ()
meteorStormRec2 n = do
  meteorStormRec2 (n - 1)
  meteorStorm

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
  voidLoggerRt <- R.createVoidLoggerRuntime
  loggerRt <- R.createLoggerRuntime loggerCfg
  coreRt <- R.createCoreRuntime voidLoggerRt

  let ops = 100000

  let actions = sequence $ replicate ops meteorStorm
  void $ runReaderT (meteorStormRec ops) coreRt
  void $ runReaderT (meteorStormRec2 ops) coreRt
  void $ runReaderT actions coreRt
