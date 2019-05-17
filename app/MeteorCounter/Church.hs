module Church where

import qualified Data.Map      as Map
import qualified Data.Set      as Set

import qualified Hydra.ChurchL as L
import qualified Hydra.Domain  as D
import           Hydra.Prelude
import qualified Hydra.Runtime as R
import           Types

delayFactor :: Int
delayFactor = 100

initState :: AppConfig -> L.StateL AppState
initState cfg = do
  ne <- L.newVar Set.empty
  nw <- L.newVar Set.empty
  se <- L.newVar Set.empty
  sw <- L.newVar Set.empty

  let catalogue = Map.fromList
        [ (NorthEast, ne)
        , (NorthWest, nw)
        , (SouthEast, se)
        , (SouthWest, sw)
        ]

  publised <- L.newVar Set.empty
  total    <- L.newVar 0
  pure $ AppState catalogue total publised cfg

getRandomMeteor :: Region -> L.RandomL Meteor
getRandomMeteor region = do
  size <- L.getRandomInt (1, 100)
  mass <- L.getRandomInt (size * 1000, size * 10000)
  pure $ Meteor size mass region

getRandomMilliseconds :: L.LangL Int
getRandomMilliseconds = (* delayFactor) <$> L.getRandomInt (0, 3000)

withRandomDelay :: AppState -> L.LangL () -> L.LangL ()
withRandomDelay st action = do
  when (delaysEnabled st) $ getRandomMilliseconds >>= L.delay
  action

publishMeteor :: AppState -> Meteor -> L.LangL ()
publishMeteor st meteor =
  L.atomically $ L.modifyVar (_channel st) $ Set.insert meteor

meteorShower :: AppState -> Region -> L.LangL ()
meteorShower st region = do
  meteor <- L.evalRandom $ getRandomMeteor region
  L.logInfo $ "New meteor discovered: " <> show meteor
  publishMeteor st meteor

trackMeteor :: AppState -> Meteor -> L.LangL ()
trackMeteor st meteor = do
  let region = _region meteor
  case Map.lookup region (_catalogue st) of
    Nothing -> L.logError $ "Region not found: " <> show region
    Just r  -> do
      L.atomically $ L.modifyVar r $ Set.insert meteor
      L.logInfo $ "New meteor tracked: " <> show meteor

meteorCounter :: AppState -> L.LangL ()
meteorCounter st = do
  untracked <- L.atomically $ do
    ps <- L.readVar (_channel st)
    when (Set.null ps) L.retry
    L.writeVar (_channel st) Set.empty
    pure $ Set.toList ps
  mapM_ (trackMeteor st) untracked

  L.atomically $ L.modifyVar (_totalMeteors st) $ (+(length untracked))
  total <- L.readVarIO (_totalMeteors st)
  L.logInfo $ "Total tracked: " <> show total

meteorsMonitoring :: AppConfig -> L.AppL ()
meteorsMonitoring cfg = do
  L.logInfo "Starting app..."
  st <- L.atomically $ initState cfg
  L.process $ forever $ meteorCounter st
  L.process $ forever $ withRandomDelay st $ meteorShower st NorthEast
  L.process $ forever $ withRandomDelay st $ meteorShower st NorthWest
  L.process $ forever $ withRandomDelay st $ meteorShower st SouthEast
  L.process $ forever $ withRandomDelay st $ meteorShower st SouthWest

scenario :: R.CoreRuntime -> AppConfig -> IO ()
scenario coreRt cfg = void
  $ R.startApp coreRt
  $ L.foreverAppChurch
  $ meteorsMonitoring cfg
