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

type MTime = Int

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

type Meteors = D.StateVar (Map.Map MTime Meteor)

type Catalogue = Map.Map Region Meteors

data AppState = AppState
  { catalogue :: D.StateVar Catalogue
  }

type AppType m a = ReaderT R.CoreRuntime m a

initState :: (MonadIO m, L.LangL m) => AppType m AppState
initState = L.atomically $ do
  ne <- L.newVar Map.empty
  nw <- L.newVar Map.empty
  se <- L.newVar Map.empty
  sw <- L.newVar Map.empty
  let catalogueMap = Map.fromList
        [ (NorthEast, ne)
        , (NorthWest, nw)
        , (SouthEast, se)
        , (SouthWest, sw)
        ]
  catalogue <- L.newVar catalogueMap
  pure $ AppState catalogue

meteorCounter :: (L.LangL m, MonadIO m) => AppState -> AppType m ()
meteorCounter st = do
  void $ readFile "abc"
  pure ()

getRandomMeteor :: (MonadIO m, L.RandomL m) => AppType m Meteor
getRandomMeteor = Meteor <$> L.getRandomInt (1, 100)

getRandomMilliseconds :: (MonadIO m, L.RandomL m) => AppType m MTime
getRandomMilliseconds = (* 1000) <$> L.getRandomInt (0, 3000)

meteorShower :: (MonadIO m, L.LangL m) => AppState -> Region -> AppType m ()
meteorShower st region = do
  getRandomMilliseconds >>= L.delay
  meteor <- getRandomMeteor
  L.logInfo $ "[MS] " <> " a new meteor appeared at " <> show region <> ": " <> show meteor
  meteorShower st region

meteorsMonitoring :: (MonadIO m, L.LangL m) => AppType m ()
meteorsMonitoring = do
  L.logInfo "Starting app..."
  L.logInfo "Delaying..."
  L.delay 10000
  L.logInfo "Done."
  st <- initState
  -- liftIO $ forkIO $ meteorCounter st

  -- liftIO $ forkIO $ meteorCounter st
  -- L.forkProcess $ meteorShower st NorthEast
  -- L.forkProcess $ meteorShower st NorthWest
  -- L.forkProcess $ meteorShower st SouthEast
  -- L.forkProcess $ meteorShower st SouthWest
  pure ()


loggerCfg :: D.LoggerConfig
loggerCfg = D.LoggerConfig
  { D._format       = "$prio $loggername: $msg"
  , D._level        = D.Debug
  , D._logFilePath  = ""
  , D._logToConsole = True
  , D._logToFile    = False
  }

-- main :: IO ()
-- main = do
--   loggerRt <- R.createLoggerRuntime loggerCfg
--   appRt <- R.createAppRuntime loggerRt
--   -- R.startApp appRt $ L.foreverApp meteorsMonitoring
--   runReaderT (L.foreverApp meteorsMonitoring) appRt

delayAction :: (MonadIO m, L.ControlFlowL m) => Int -> AppType m ()
delayAction = L.delay


-- Could not decuce...
-- initStateApp :: L.LangL m => m AppState
-- initStateApp = L.atomically initState

-- This is wrong: the upper m should not be a state-working m
-- (because the state is STM, and should not appear as is, only with atomically).
-- Also,
-- 'No instance for (L.StateL (ReaderT R.CoreRuntime IO))':
-- this interpreter should not exist (we don't want to evaluate the actions separately in IO)

main :: IO ()
main = do
  loggerRt <- R.createLoggerRuntime loggerCfg
  coreRt <- R.createCoreRuntime loggerRt
  runReaderT meteorsMonitoring coreRt
  void $ runReaderT initState coreRt