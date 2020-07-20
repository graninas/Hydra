{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FTL where

import           Control.Monad
import           Hydra.Prelude

import qualified Hydra.FTL     as FTL
import qualified "hydra-base" Hydra.Runtime as R

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
