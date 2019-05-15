{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Church where

import           Control.Monad
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import           Hydra.Prelude

import qualified Hydra.ChurchL  as L
import qualified Hydra.Domain  as D
import qualified Hydra.Runtime as R
import           Types


getRandomMeteor :: L.RandomL Meteor
getRandomMeteor = Meteor <$> L.getRandomInt (1, 100)

getRandomRegion :: L.RandomL Region
getRandomRegion = toRegion <$> L.getRandomInt (1, 4)
  where
    toRegion 1 = NorthWest
    toRegion 2 = NorthEast
    toRegion 3 = SouthWest
    toRegion _ = SouthEast

createMeteor :: L.LangL (Meteor, Region)
createMeteor = do
  meteor <- L.evalRandom getRandomMeteor
  region <- L.evalRandom getRandomRegion
  pure (meteor, region)

meteorStorm :: L.AppL ()
meteorStorm = do
  (meteor, region) <- L.scenario createMeteor
  L.logInfo $ "[MS] " <> " a new meteor appeared at " <> show region <> ": " <> show meteor

-- Tail-rec
meteorStormRec :: Int -> L.AppL ()
meteorStormRec 0 = pure ()
meteorStormRec n = do
  meteorStorm
  meteorStormRec (n - 1)

-- Not tail-rec
meteorStormRec2 :: Int -> L.AppL ()
meteorStormRec2 0 = pure ()
meteorStormRec2 n = do
  meteorStormRec2 (n - 1)
  meteorStorm

scenario1, scenario2, scenario3 :: Int -> R.CoreRuntime -> IO ()
scenario1 ops coreRt = void $ R.startApp coreRt (meteorStormRec ops)
scenario2 ops coreRt = void $ R.startApp coreRt (meteorStormRec2 ops)
scenario3 ops coreRt = void $ R.startApp coreRt actions
  where
    actions = replicateM ops meteorStorm
