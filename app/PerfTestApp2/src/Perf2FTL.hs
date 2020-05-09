{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Perf2FTL where

import           Control.Monad
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import           Hydra.Prelude

import qualified Hydra.Domain  as D
import qualified Hydra.FTL     as FTL
import qualified Hydra.Runtime as R

import           Hydra.FTLI    ()


flow :: (MonadIO m, FTL.LangL m) => IORef Int -> m ()
flow ref = do
  val' <- liftIO $ readIORef ref
  val <- FTL.getRandomInt (1, 100)
  liftIO $ writeIORef ref $ val' + val

scenario :: Int -> R.CoreRuntime -> IO ()
scenario ops coreRt = do
  ref <- newIORef 0
  void $ runReaderT (replicateM_ ops $ flow ref) coreRt
  val <- readIORef ref
  print val
