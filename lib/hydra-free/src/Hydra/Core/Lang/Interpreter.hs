{-# LANGUAGE BangPatterns #-}

module Hydra.Core.Lang.Interpreter where

import           Hydra.Prelude

import qualified Data.Map as Map
import           Control.Exception (throwIO)
import qualified Data.Aeson as A
import qualified Servant.Client                             as S
import qualified Network.Socket                             as Sock hiding (recv, send)
import qualified Network.Socket.ByteString.Lazy             as Sock
import qualified Data.Text as T

import           Hydra.Core.ControlFlow.Interpreter         (runControlFlowL)
import qualified Hydra.Core.Language                        as L
import           Hydra.Core.Logger.Impl.HsLoggerInterpreter (runLoggerL, flushStmLogger)
import           Hydra.Core.Random.Interpreter              (runRandomL)
import qualified Hydra.Core.RLens                           as RLens
import qualified Hydra.Runtime                              as R
import qualified Hydra.Domain                               as D
import           Hydra.Core.State.Interpreter               (runStateL)
import           Hydra.Core.KVDB.Interpreter                (runAsRocksDBL, runAsRedisL)
import           Hydra.Core.SqlDB.Interpreter               (runSqlDBL)
import qualified Hydra.Core.Networking.Internal.Socket      as ISock

import qualified Data.ByteString.Lazy as LBS

evalRocksKVDB'
  :: R.CoreRuntime
  -> String
  -> L.KVDBL db a
  -> IO a
evalRocksKVDB' coreRt dbname act = do
  dbs <- atomically $ readTMVar $ coreRt ^. RLens.rocksDBs
  case Map.lookup dbname dbs of
    Nothing       -> error $ "Rocks KV DB not registered: " +| dbname |+ ""
    Just dbHandle -> runAsRocksDBL dbHandle act

evalRedisKVDB'
  :: R.CoreRuntime
  -> String
  -> L.KVDBL db a
  -> IO a
evalRedisKVDB' coreRt dbname act = do
  conns <- atomically $ readTMVar $ coreRt ^. RLens.redisConns
  case Map.lookup dbname conns of
    Nothing   -> error $ "Redis KV DB not registered: " +| dbname |+ ""
    Just conn -> runAsRedisL conn act

evalKVDB'
  :: R.CoreRuntime
  -> D.DBHandle db
  -> L.KVDBL db a
  -> IO a
evalKVDB' coreRt (D.DBHandle D.RocksDB dbname) act =
  evalRocksKVDB' coreRt dbname act
evalKVDB' coreRt (D.DBHandle D.Redis   dbname) act =
  evalRedisKVDB' coreRt dbname act

interpretLangF :: R.CoreRuntime -> L.LangF a -> IO a
interpretLangF coreRt (L.EvalStateAtomically action next) = do
  let stateRt = coreRt ^. RLens.stateRuntime
  let logHndl = coreRt ^. RLens.loggerRuntime . RLens.hsLoggerHandle
  res <- atomically $ runStateL stateRt action
  flushStmLogger (stateRt ^. RLens.stmLog) logHndl
  pure $ next res
interpretLangF coreRt (L.EvalLogger loggerAct next) =
  next <$> runLoggerL (coreRt ^. RLens.loggerRuntime . RLens.hsLoggerHandle) loggerAct
interpretLangF _      (L.EvalRandom  s next)        = next <$> runRandomL s
interpretLangF coreRt (L.EvalControlFlow f    next) = next <$> runControlFlowL coreRt f
interpretLangF _      (L.EvalIO f next)             = do
  !r <- f
  pure $ next r
interpretLangF coreRt (L.EvalKVDB storage act next) = next <$> evalKVDB' coreRt storage act
interpretLangF coreRt (L.EvalSqlDB conn sqlDbMethod next) = do

  -- TODO: Debug logging beam function is known to be slow.
  let dbgLogger = runLoggerL (coreRt ^. RLens.loggerRuntime . RLens.hsLoggerHandle)
                . L.logMessage D.Debug . show

  eRes <- R.withTransaction conn $ \nativeConn ->
      runSqlDBL nativeConn dbgLogger sqlDbMethod
  pure $ next eRes

interpretLangF coreRt (L.GetSqlDBConnection cfg next) = do
  let connTag = D.getConnTag cfg
  let connsVar = coreRt ^. RLens.sqlConns
  connMap <- readMVar connsVar
  case Map.lookup connTag connMap of
    Just conn -> pure $ next $ Right $ R.runtimeToBem connTag conn
    Nothing -> pure
      $ next $ Left $ D.DBError D.ConnectionDoesNotExist
      $ "Connection for " <> show connTag <> " not found."

interpretLangF _      (L.ThrowException exc _) = throwIO exc

interpretLangF coreRt (L.RunSafely act next) = do
  eResult <- try $ runLangL coreRt act
  pure $ next $ case eResult of
    Left err -> Left err
    Right r  -> Right r

interpretLangF coreRt (L.CallServantAPI bUrl clientAct next)
  = next <$> catchAny
      (S.runClientM clientAct (S.mkClientEnv (coreRt ^. RLens.httpClientManager) bUrl))
      (pure . Left . S.ConnectionError)

interpretLangF _ (L.CallRPC (D.Address host port) req next) =
  next <$> catchAny (do
    address <- head <$> Sock.getAddrInfo Nothing (Just $ T.unpack host) (Just $ show port)
    sock    <- Sock.socket (Sock.addrFamily address) Sock.Stream Sock.defaultProtocol
    finally (do
      Sock.connect sock $ Sock.addrAddress address
      -- ISock.sendDatagram sock $ LBS.toStrict $ A.encode req
      ISock.sendDatagram sock $ A.encode req
      msg <- ISock.receiveDatagram sock
      pure $ transformEither T.pack id $ A.eitherDecode msg
      ) (Sock.close sock)
  ) (pure . Left . show)

runLangL :: R.CoreRuntime -> L.LangL a -> IO a
runLangL coreRt = foldFree (interpretLangF coreRt)


transformEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d
transformEither f _ (Left  a) = Left (f a)
transformEither _ f (Right a) = Right (f a)
