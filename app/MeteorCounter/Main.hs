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
  , _published :: D.StateVar (Set.Set (Region, Meteor))
  }


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
  pure $ AppState catalogue publised

getRandomMeteor :: L.LangL Meteor
getRandomMeteor = do
  size <- L.getRandomInt (1, 100)
  mass <- L.getRandomInt (size * 1000, size * 10000)
  pure $ Meteor size mass

getRandomMilliseconds :: L.LangL Time
getRandomMilliseconds = (* 1000) <$> L.getRandomInt (0, 3000)

withRandomDelay :: L.LangL () -> L.LangL ()
withRandomDelay action = do
  getRandomMilliseconds >>= L.delay
  action

publishMeteor :: AppState -> (Region, Meteor) -> L.LangL ()
publishMeteor st meteor =
  L.atomically $ L.modifyVar (_published st) $ Set.insert meteor

meteorShower :: AppState -> Region -> L.LangL ()
meteorShower st region = do
  meteor <- getRandomMeteor
  L.logInfo $ "A new meteor appeared at " <> show region <> ": " <> show meteor
  publishMeteor st (region, meteor)

countMeteor :: AppState -> (Region, Meteor) -> L.LangL ()
countMeteor st (region, meteor) =
  case Map.lookup region (_catalogue st) of
    Nothing -> L.logError $ "Region not found: " <> show region
    Just r  -> do
      L.atomically $ L.modifyVar r $ Set.insert meteor
      L.logInfo $ "A new meteor tracked at " <> show region <> ": " <> show meteor

meteorCounter :: AppState -> L.LangL ()
meteorCounter st = do
  untracked <- L.atomically $ do
    ps <- L.readVar (_published st)
    L.writeVar (_published st) Set.empty
    pure $ Set.toList ps
  mapM_ (countMeteor st) untracked

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
