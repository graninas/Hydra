module FTL where

import qualified Data.Map            as Map
import qualified Data.Set            as Set

import           FTLTypes
import qualified Hydra.Domain        as D
import qualified Hydra.FTL           as L
import           Hydra.Prelude
import qualified Hydra.Runtime       as R
import           Types

import qualified UnliftIO.Concurrent as UIO

delayFactor :: Int
delayFactor = 100

initState :: AppConfig -> STM AppState'
initState cfg = do
  ne <- newTVar Set.empty
  nw <- newTVar Set.empty
  se <- newTVar Set.empty
  sw <- newTVar Set.empty

  let catalogue = Map.fromList
        [ (NorthEast, ne)
        , (NorthWest, nw)
        , (SouthEast, se)
        , (SouthWest, sw)
        ]

  publised <- newTVar Set.empty
  total    <- newTVar 0
  pure $ AppState' catalogue total publised cfg

getRandomMeteor :: L.RandomL m => Region -> m Meteor
getRandomMeteor region = do
  size <- L.getRandomInt (1, 100)
  mass <- L.getRandomInt (size * 1000, size * 10000)
  pure $ Meteor size mass region

getRandomMilliseconds :: L.RandomL m => m Int
getRandomMilliseconds = (* delayFactor) <$> L.getRandomInt (0, 3000)

withRandomDelay
  :: (L.ControlFlowL m, L.RandomL m)
  => AppState' -> m () -> m ()
withRandomDelay st action = do
  when (delaysEnabled' st) $ getRandomMilliseconds >>= L.delay
  action

publishMeteor :: MonadIO m => AppState' -> Meteor -> m ()
publishMeteor st meteor =
  atomically $ modifyTVar (_channel' st) $ Set.insert meteor

meteorShower
  :: (MonadIO m, L.LoggerL m, L.RandomL m)
  => AppState' -> Region -> m ()
meteorShower st region = do
  meteor <- getRandomMeteor region
  when (doLogDiscovered' st) $ L.logInfo $ "New meteor discovered: " <> show meteor
  publishMeteor st meteor

trackMeteor
  :: (MonadIO m, L.LoggerL m)
  => AppState' -> Meteor -> m ()
trackMeteor st meteor = do
  let region = _region meteor
  case Map.lookup region (_catalogue' st) of
    Nothing -> L.logError $ "Region not found: " <> show region
    Just r  -> do
      atomically $ modifyTVar r $ Set.insert meteor
      when (doLogTracked' st) $ L.logInfo $ "New meteor tracked: " <> show meteor

meteorCounter :: (MonadIO m, L.LoggerL m) => AppState' -> m ()
meteorCounter st = do
  untracked <- atomically $ do
    ps <- readTVar (_channel' st)
    when (Set.null ps) retry
    writeTVar (_channel' st) Set.empty
    pure $ Set.toList ps
  mapM_ (trackMeteor st) untracked

  atomically $ modifyTVar (_totalMeteors' st) $ (+(length untracked))
  total <- readTVarIO (_totalMeteors' st)

  when (doLogTotal' st) $ L.logInfo $ "Total tracked: " <> show total

meteorsMonitoring :: (MonadIO m, L.LoggerL m) => AppConfig -> m ()
meteorsMonitoring cfg = do
  st <- atomically $ initState cfg

  forkIO $ forever $ meteorCounter st
  forkIO $ forever $ withRandomDelay st $ meteorShower st NorthEast
  forkIO $ forever $ withRandomDelay st $ meteorShower st NorthWest
  forkIO $ forever $ withRandomDelay st $ meteorShower st SouthEast
  forkIO $ forever $ withRandomDelay st $ meteorShower st SouthWest

  atomically $ do
    let maxTotal = fromMaybe 0 $ maxMeteors cfg
    total <- readTVar $ _totalMeteors' st
    when (maxTotal == 0 || total < maxTotal) retry

-- scenario :: R.CoreRuntime -> AppConfig -> IO ()
-- scenario coreRt cfg
--   = void
--   $ R.startApp coreRt
--   $ meteorsMonitoring cfg
