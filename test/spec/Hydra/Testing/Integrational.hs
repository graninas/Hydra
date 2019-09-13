{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Hydra.Testing.Integrational where

import           Data.Aeson
import qualified Data.ByteString.Lazy                       as LBS
import qualified Data.Map                                   as M
import qualified "rocksdb-haskell" Database.RocksDB         as Rocks
import qualified Hydra.Domain                               as D
import qualified Hydra.Interpreters                         as I
import qualified Hydra.Language                             as L
import           Hydra.Prelude
import qualified Hydra.Runtime                              as R
import qualified System.Directory                           as Dir
import           System.FilePath                            as FP ((</>))
import qualified System.FilePath                            as Dir

testLogFilePath :: IsString a => a
testLogFilePath = "/tmp/log/test.log"

consoleLoggerConfig :: D.LoggerConfig
consoleLoggerConfig = D.LoggerConfig
    { D._format       = "$prio $loggername: $msg"
    , D._level        = D.Debug
    , D._logFilePath  = testLogFilePath
    , D._logToConsole = True
    , D._logToFile    = False
    }

testConfigFilePath :: IsString a => a
testConfigFilePath = "./configs/tst_client_test_config.json"

evalApp :: L.AppL a -> IO a
evalApp app = R.withAppRuntime Nothing $ \appRt -> R.startApp appRt app

mkTestPath :: FilePath -> IO FilePath
mkTestPath testName = do
  hd <- Dir.getHomeDirectory
  pure $ hd </> ".hydra" </> testName

rmTestPath :: FilePath -> IO ()
rmTestPath path = do
  whenM (Dir.doesDirectoryExist path) $ Dir.removePathForcibly path
  whenM (Dir.doesDirectoryExist path) $ error "Can't delete path."


mkEmptyRocksDb :: FilePath -> IO ()
mkEmptyRocksDb dbPath = do
  rmTestPath dbPath
  Dir.createDirectoryIfMissing True dbPath
  -- This creates an empty DB to get correct files in the directory.
  let opening = Rocks.open dbPath $ Rocks.defaultOptions { Rocks.createIfMissing = True
                                                         , Rocks.errorIfExists   = False
                                                         }
  bracket opening Rocks.close (const (pure ()))

withRocksDbAbsence :: FilePath -> IO a -> IO ()
withRocksDbAbsence dbPath act = do
  rmTestPath dbPath
  void act `finally` rmTestPath dbPath

withRocksDbPresence :: FilePath -> IO a -> IO ()
withRocksDbPresence dbPath act = do
  mkEmptyRocksDb dbPath
  void act `finally` rmTestPath dbPath
