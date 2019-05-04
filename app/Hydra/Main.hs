module Main where

import qualified Data.Map       as Map
import qualified Data.Set       as Set

import qualified Hydra.Domain   as D
import qualified Hydra.FTL as FTL
-- import qualified Hydra.Language as L
import           Hydra.Prelude
import qualified Hydra.Runtime  as R

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


initState :: (L.AppL m, L.StateL m) => ReaderT AppState m
initState = do
  ne <- L.newVarIO Map.empty
  nw <- L.newVarIO Map.empty
  se <- L.newVarIO Map.empty
  sw <- L.newVarIO Map.empty
  let catalogueMap = Map.fromList
        [ (NorthEast, ne)
        , (NorthWest, nw)
        , (SouthEast, se)
        , (SouthWest, sw)
        ]
  catalogue <- L.newVarIO catalogueMap
  pure $ AppState catalogue

meteorCounter :: AppState -> L.LangL ()
meteorCounter st = pure ()

getRandomMeteor :: L.LangL Meteor
getRandomMeteor = Meteor <$> L.getRandomInt (1, 100)

getRandomMilliseconds :: L.LangL MTime
getRandomMilliseconds = (* 1000) <$> L.getRandomInt (0, 3000)

meteorShower :: AppState -> Region -> L.LangL ()
meteorShower st region = do
  getRandomMilliseconds >>= L.delay
  meteor <- getRandomMeteor
  L.logInfo $ "[MS] " <> " a new meteor appeared at " <> show region <> ": " <> show meteor
  meteorShower st region

meteorsMonitoring :: L.AppL ()
meteorsMonitoring = do
  L.logInfo "Starting app..."
  st <- initState
  L.process $ meteorCounter st
  L.process $ meteorShower st NorthEast
  L.process $ meteorShower st NorthWest
  L.process $ meteorShower st SouthEast
  L.process $ meteorShower st SouthWest

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
  appRt <- R.createAppRuntime loggerRt
  R.startApp appRt $ L.foreverApp meteorsMonitoring
