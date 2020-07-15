{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Church where

import           Control.Monad
import           Hydra.Prelude

import "hydra-church-free" qualified Hydra.Language as L
import qualified Hydra.Runtime as R

flow :: IORef Int -> L.AppL ()
flow ref = L.scenario $ do
  val' <- L.evalIO $ readIORef ref
  val  <- L.getRandomInt (1, 100)
  L.evalIO $ writeIORef ref $ val' + val


scenario :: Int -> R.AppRuntime -> IO ()
scenario ops appRt = do
  ref <- newIORef 0
  void $ R.startApp appRt (replicateM_ ops $ flow ref)
  val <- readIORef ref
  print val
