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

createAppRuntime :: R.LoggerRuntime -> IO R.AppRuntime
createAppRuntime loggerRuntime = R.createAppRuntime loggerRuntime

evalApp :: L.AppL a -> IO a
evalApp app = do
  rt <- R.createVoidLoggerRuntime >>= createAppRuntime
  res <- R.startApp rt app
  -- R.clearAppRuntime rt
  pure res

mkDbPath :: FilePath -> IO FilePath
mkDbPath testName = do
  hd <- Dir.getHomeDirectory
  pure $ hd </> ".hydra" </> testName

rmDb :: FilePath -> IO ()
rmDb dbPath = do
  whenM (Dir.doesDirectoryExist dbPath) $ Dir.removePathForcibly dbPath
  whenM (Dir.doesDirectoryExist dbPath) $ error "Can't delete db."

mkDb :: FilePath -> IO ()
mkDb dbPath = do
  rmDb dbPath
  Dir.createDirectoryIfMissing True dbPath
  -- This creates an empty DB to get correct files in the directory.
  let opening = Rocks.open dbPath $ Rocks.defaultOptions { Rocks.createIfMissing = True
                                                         , Rocks.errorIfExists   = False
                                                         }
  bracket opening Rocks.close (const (pure ()))

withDbAbsence :: FilePath -> IO a -> IO ()
withDbAbsence dbPath act = do
  rmDb dbPath
  void act `finally` rmDb dbPath

withDbPresence :: FilePath -> IO a -> IO ()
withDbPresence dbPath act = do
  mkDb dbPath
  void act `finally` rmDb dbPath
