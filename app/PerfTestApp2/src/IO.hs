{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module IO where

import           Control.Monad
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import           Hydra.Prelude

import           System.Entropy
import           System.Random       hiding (next)

flow :: IORef Int -> IO ()
flow ref = do
  val' <- readIORef ref
  val <- randomRIO (1, 100)
  writeIORef ref $ val' + val

scenario :: Int -> IO ()
scenario ops = do
  ref <- newIORef 0
  void $ replicateM_ ops $ flow ref
  val <- readIORef ref
  print val
