module Main where

import qualified Data.Map       as Map
import qualified Data.Set       as Set

import qualified Hydra.Domain   as D
import qualified Hydra.Language as L
import           Hydra.Prelude
import qualified Hydra.Runtime  as R

type Time = Int

data Meteor = Meteor
  { _size :: Int
  , _mass :: Int
  }
  deriving (Show, Eq, Ord)

data Region
  = NorthEast
  | NorthWest
  | SouthEast
  | SouthWest
  deriving (Show, Eq, Ord)

type Meteors = D.StateVar (Set.Set Meteor)

type Catalogue = Map.Map Region Meteors

data AppState = AppState
  { _catalogue :: Catalogue
  , _totalMeteors :: D.StateVar Int
  , _discoveryChannel :: D.StateVar (Set.Set (Region, Meteor))
  }

delayFactor :: Int
delayFactor = 100

initState :: L.AppL AppState
initState = do
  ne <- L.newVarIO Set.empty
  nw <- L.newVarIO Set.empty
  se <- L.newVarIO Set.empty
  sw <- L.newVarIO Set.empty

  let catalogue = Map.fromList
        [ (NorthEast, ne)
        , (NorthWest, nw)
        , (SouthEast, se)
        , (SouthWest, sw)
        ]

  publised <- L.newVarIO Set.empty
  total    <- L.newVarIO 0
  pure $ AppState catalogue total publised

getRandomMeteor :: L.LangL Meteor
getRandomMeteor = do
  size <- L.getRandomInt (1, 100)
  mass <- L.getRandomInt (size * 1000, size * 10000)
  pure $ Meteor size mass

getRandomMilliseconds :: L.LangL Time
getRandomMilliseconds = (* delayFactor) <$> L.getRandomInt (0, 3000)

withRandomDelay :: L.LangL () -> L.LangL ()
withRandomDelay action = do
  getRandomMilliseconds >>= L.delay
  action

publishMeteor :: AppState -> (Region, Meteor) -> L.LangL ()
publishMeteor st meteor =
  L.atomically $ L.modifyVar (_discoveryChannel st) $ Set.insert meteor

meteorShower :: AppState -> Region -> L.LangL ()
meteorShower st region = do
  meteor <- getRandomMeteor
  L.logInfo $ "New meteor discovered: " <> show (region, meteor)
  publishMeteor st (region, meteor)

trackMeteor :: AppState -> (Region, Meteor) -> L.LangL ()
trackMeteor st rm@(region, meteor) =
  case Map.lookup region (_catalogue st) of
    Nothing -> L.logError $ "Region not found: " <> show region
    Just r  -> do
      L.atomically $ L.modifyVar r $ Set.insert meteor
      L.logInfo $ "New meteor tracked: " <> show rm

meteorCounter :: AppState -> L.LangL ()
meteorCounter st = do
  untracked <- L.atomically $ do
    ps <- L.readVar (_discoveryChannel st)
    when (Set.null ps) L.retry
    L.writeVar (_discoveryChannel st) Set.empty
    pure $ Set.toList ps
  mapM_ (trackMeteor st) untracked

  L.atomically $ L.modifyVar (_totalMeteors st) $ (+(length untracked))
  total <- L.readVarIO (_totalMeteors st)
  L.logInfo $ "Total tracked: " <> show total

meteorsMonitoring :: L.AppL ()
meteorsMonitoring = do
  L.logInfo "Starting app..."
  st <- initState
  L.process $ forever $ meteorCounter st
  L.process $ forever $ withRandomDelay $ meteorShower st NorthEast
  L.process $ forever $ withRandomDelay $ meteorShower st NorthWest
  L.process $ forever $ withRandomDelay $ meteorShower st SouthEast
  L.process $ forever $ withRandomDelay $ meteorShower st SouthWest

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

  loggerRt <- R.createLoggerRuntime loggerCfg
  coreRt <- R.createCoreRuntime loggerRt
  R.startApp coreRt $ L.foreverApp meteorsMonitoring
