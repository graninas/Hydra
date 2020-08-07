{-# LANGUAGE PackageImports #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           "hydra-base" Hydra.Prelude

import qualified "hydra-base" Hydra.Runtime              as R
import qualified "hydra-free" Hydra.Interpreters         as R

import           Astro.Config               (loggerCfg)
import           Astro.ConsoleOptions
import           Astro.Server               (runAstroServer)
import           Astro.Client.Common        (ReportChannel(..), Approach(..))
import qualified Astro.Client.ServiceHandle as SH
import qualified Astro.Client.ReaderT       as RT
import qualified Astro.Client.FreeMonad     as FM
import qualified Astro.Client.FinalTagless  as FT
import qualified Astro.Client.FinalTagless2 as FT2
import qualified Astro.Client.GADT          as GADT

-- how many beats per minute

-- b / m
--
-- b ~ n seconds
--
-- 60 sec / bpm
--
-- bpm = 120

-- frequency = bps = 120 / 1 min (60 sec) = 2 beats per second
-- duration = 1 second / (2 bps) = 0.5


frequency :: Int -> Float
frequency bpm = (fromIntegral bpm) / 60.0

bps :: Int -> Float
bps = frequency

duration :: Int -> Float
duration bpm = 1.0 / (frequency bpm)



printingWorker :: Int -> IO ()
printingWorker ms = do
  putStrLn @Text "Hello, world!"
  threadDelay ms
  printingWorker ms


-- size: 4 / 4
--
-- 4 beats of a quarter size
-- if the duration is 0.5 then a beat should appear after 0.5 sec
--
-- 1 / 8 note
--
-- 1 / 8 + 1 / 4


main :: IO ()
main = do

  putStrLn @Text $ show $ duration 120

  -- forkIO $ printingWorker $ 1000 * 1000

  input <- getLine

  putStrLn input
