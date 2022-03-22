{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FTL where

import qualified Data.Map                as Map
import qualified Data.Set                as Set

import           FTLTypes
import qualified Hydra.FTL               as L
import           Hydra.Prelude
import qualified Hydra.Runtime           as R
import           Types

import           Hydra.FTLI              ()

-- Flaws of FT
-- - BL depends on Runtime
-- - BL and implementation works in the same Runtime
-- - Implementation details leak into BL through type classes
--   (see logger type classes with additional type variables)
-- - Advanced language features (type classes + Type Families)
-- - Single runtime for all language interpreters
-- - Implicit type class instances (not values)
-- -

newtype AppM a = AppM { runAppM :: ReaderT R.CoreRuntime IO a }
  deriving (Functor, Applicative, Monad, L.ControlFlowL, L.LoggerL, L.RandomL, L.ProcessL)

class
  ( L.StateL (L.Transaction m)
  , L.Atomic m
  , L.LoggerL m
  , L.RandomL m
  , L.ControlFlowL m
  , L.ProcessL m
  ) => Lang m
instance Lang AppM

instance L.Atomic AppM where
  type Transaction AppM = STM
  transaction = AppM . atomically

delayFactor :: Int
delayFactor = 100

initState :: L.StateL m => AppConfig -> m (AppState' m)
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
  pure $ AppState' catalogue total publised cfg

getRandomMeteor :: L.RandomL m => Region -> m Meteor
getRandomMeteor region = do
  size <- L.getRandomInt (1, 100)
  mass <- L.getRandomInt (size * 1000, size * 10000)
  pure $ Meteor size mass region

getRandomMilliseconds :: L.RandomL m => m Int
getRandomMilliseconds = L.getRandomInt (0, 3000)

withRandomDelay :: (L.ControlFlowL m, L.RandomL m) => AppState' t -> m () -> m ()
withRandomDelay st action = do
  when (delaysEnabled' st)
    $ getRandomMilliseconds >>= \d -> L.delay $ d * dFactor' st
  action

publishMeteor :: L.StateL m => AppState' m -> Meteor -> m ()
publishMeteor st meteor =
  L.modifyVar (_channel' st) $ Set.insert meteor

meteorShower :: Lang m => AppState' (L.Transaction m) -> Region -> m ()
meteorShower st region = do
  meteor <- getRandomMeteor region
  when (doLogDiscovered' st) $ L.logInfo $ "New meteor discovered: " <> show meteor
  L.transaction $ publishMeteor st meteor

trackMeteor :: Lang m => AppState' (L.Transaction m) -> Meteor -> m ()
trackMeteor st meteor = do
  let region = _region meteor
  case Map.lookup region (_catalogue' st) of
    Nothing -> L.logError $ "Region not found: " <> show region
    Just r  -> do
      when (storeTrackedMeteors' st) $
        L.transaction $ L.modifyVar r $ Set.insert meteor
      when (doLogTracked' st) $ L.logInfo $ "New meteor tracked: " <> show meteor

meteorCounter :: Lang m => AppState' (L.Transaction m) -> m ()
meteorCounter st = do
  untracked <- L.transaction $ do
     ps <- L.readVar (_channel' st)
     when (Set.null ps) L.retry
     L.writeVar (_channel' st) Set.empty
     pure $ Set.toList ps
  mapM_ (trackMeteor st) untracked

  L.transaction $ L.modifyVar (_totalMeteors' st) $ (+(length untracked))
  total <- L.transaction $ L.readVar (_totalMeteors' st)

  when (doLogTotal' st) $ L.logInfo $ "Total tracked: " <> show total

meteorsMonitoring :: (Lang m, L.Transaction m ~ t) => AppConfig -> AppState' t -> m ()
meteorsMonitoring cfg st = do
  _ <- L.forkProcess $ forever $ meteorCounter st
  _ <- L.forkProcess $ forever $ withRandomDelay st $ meteorShower st NorthEast
  _ <- L.forkProcess $ forever $ withRandomDelay st $ meteorShower st NorthWest
  _ <- L.forkProcess $ forever $ withRandomDelay st $ meteorShower st SouthEast
  _ <- L.forkProcess $ forever $ withRandomDelay st $ meteorShower st SouthWest

  L.transaction $ do
    let maxTotal = fromMaybe 0 $ maxMeteors cfg
    total <- L.readVar $ _totalMeteors' st
    when (maxTotal == 0 || total < maxTotal) L.retry

scenario :: R.CoreRuntime -> AppConfig -> IO ()
scenario coreRt cfg = void $ do
   st <- atomically $ initState cfg
   runReaderT (runAppM $ meteorsMonitoring cfg st) coreRt
