module Hydra.Framework.App.Interpreter where

import           Hydra.Prelude

import qualified Hydra.Core.Domain        as D
import qualified Hydra.Core.Interpreters  as Impl
import qualified Hydra.Core.Language      as L
import qualified Hydra.Core.RLens         as RLens
import qualified Hydra.Runtime            as R
import qualified Hydra.Framework.Language as L
import qualified Hydra.Framework.RLens    as RLens

import qualified System.Console.Haskeline as HS

import qualified Data.Map as Map
import           Control.Concurrent (forkFinally)
import qualified Control.Exception.Safe as Safe
import qualified Network as N
import qualified Network.Socket as S hiding (recv)
import qualified Network.Socket.ByteString.Lazy as S
import           Data.Aeson as A

langRunner :: R.CoreRuntime -> Impl.LangRunner L.LangL
langRunner coreRt = Impl.LangRunner (Impl.runLangL coreRt)

initKVDB' :: forall db. D.DB db => R.CoreRuntime -> D.KVDBConfig db -> String -> IO (D.DBResult (D.DBHandle db))
initKVDB' coreRt cfg@(D.RocksDBConfig _ _ _) dbName =
  R.initRocksDB' (coreRt ^. RLens.rocksDBs) cfg dbName
initKVDB' coreRt cfg@(D.RedisConfig) dbName =
  R.initRedisDB' (coreRt ^. RLens.redisConns) cfg dbName

connect :: D.DBConfig beM -> IO (D.DBResult (D.SqlConn beM))
connect cfg = do
  eConn <- try $ R.createSqlConn cfg
  case eConn of
    Left (e :: SomeException) -> pure $ Left $ D.DBError D.FailedToConnect $ show e
    Right conn -> pure $ Right conn

evalCliAction :: R.CoreRuntime -> D.CliToken -> D.CliAction -> HS.InputT IO Bool
evalCliAction coreRt cliToken (D.CliFinish mbMsg) = do
  whenJust mbMsg HS.outputStrLn
  liftIO $ Impl.runLangL coreRt $ L.writeVarIO (D.cliFinishedToken cliToken) True
  pure True
evalCliAction _ _ D.CliLoop            = pure True
evalCliAction _ _ (D.CliOutputMsg msg) = HS.outputStrLn msg >> pure True

interpretAppF :: R.AppRuntime -> L.AppF a -> IO a
interpretAppF appRt (L.EvalLang action next) = do
  let coreRt = appRt ^. RLens.coreRuntime
  res <- Impl.runLangL coreRt action
  pure $ next res

interpretAppF appRt (L.EvalProcess action next) = do
  let coreRt = appRt ^. RLens.coreRuntime
  res <- Impl.runProcessL (langRunner coreRt) (coreRt ^. RLens.processRuntime) action
  pure $ next res

interpretAppF appRt (L.InitKVDB cfg dbName next) =
  next <$> initKVDB' (appRt ^. RLens.coreRuntime) cfg dbName

interpretAppF appRt (L.InitSqlDB cfg next) = do
  let connTag = D.getConnTag cfg
  let connsVar = appRt ^. RLens.coreRuntime . RLens.sqlConns
  connMap <- takeMVar connsVar
  case Map.lookup connTag connMap of
    Just _ -> do
      putMVar connsVar connMap
      pure
        $ next $ Left $ D.DBError D.ConnectionAlreadyExists
        $ "Connection for " <> show connTag <> " already created."
    Nothing -> do
      eConn <- connect cfg
      case eConn of
        Right conn -> do
          putMVar connsVar $ Map.insert connTag (R.bemToRuntime conn) connMap
          pure $ next $ Right conn
        Left err -> do
          putMVar connsVar connMap
          pure $ next $ Left err




interpretAppF appRt (L.ServeRpc port protocol next) = do
  eRes <- startRpcServer port protocol
  pure $ next eRes



interpretAppF appRt (L.CliF completeFunc onStep onUnknownCommand handlers cliToken next) = do
  let coreRt = appRt ^. RLens.coreRuntime

  handlersRef <- newIORef Map.empty
  Impl.runCliHandlerL handlersRef handlers
  handlersVal <- readIORef handlersRef

  void $ forkIO $ do
    let loop = do
          mbLine <- HS.getInputLine "> "
          let eAct = case mbLine of
                Nothing -> Left Nothing
                Just line -> case Map.lookup line handlersVal of
                  Nothing -> Left (Just line)
                  Just act -> Right act

          doLoop <- case eAct of
            Left Nothing    -> pure True
            Left (Just cmd) -> do
              cliAction <- liftIO $ runAppL appRt $ onUnknownCommand cmd
              evalCliAction coreRt cliToken cliAction
            Right action    -> do
              result    <- liftIO $ Impl.runLangL coreRt action
              cliAction <- liftIO $ runAppL appRt $ onStep result
              evalCliAction coreRt cliToken cliAction

          when doLoop loop

    let cf = HS.completeWord Nothing " \t" $ pure . completeFunc
    HS.runInputT (HS.setComplete cf HS.defaultSettings) loop

  pure $ next ()

runAppL :: R.AppRuntime -> L.AppL a -> IO a
runAppL appRt = foldFree (interpretAppF appRt)

instance R.StartApp L.AppL where
  startApp = runAppL





readRpcProtocol protocol = do
  handlersRef <- newIORef mempty
  void $ Impl.prepareRpcHandlers handlersRef protocol
  readIORef handlersRef




startRpcServer
  :: R.AppRuntime
  -> D.Port
  -> L.RpcProtocol ()
  -> IO (Either D.RpcServerError ())
startRpcServer appRt port protocol = do
  handlers <- readRpcProtocol protocol

  servers  <- takeMVar $ appRt ^. RLens.rpcServers

  -- TODO: rewrite this catchAny. Seems it doesn't clear resources in case of
  -- an exxception after N.listenOn
  catchAny
    (do
        when (Map.member port servers)
          $ Safe.throwString $ "Port " <> show port <> " is used"

        listenSockVar <- newMVar listenSock
        registeredVar <- newEmptyMVar         -- no activity until the server is registered

        listenSock <- N.listenOn (N.PortNumber port)

        -- connection acceptance worker

        acceptWorkerId <- forkIO $ do
          takeMVar registeredVar              -- no activity until the server is registered

          forever $ do
            (connSock, _) <- S.accept listenSock

            -- rpc interaction worker
            -- TODO: track each worker and close on resource cleanup
            void $ forkFinally
                (do
                    msg      <- receiveDatagram connSock
                    response <- callRpc (runLangL appRt) handlerMap msg
                    sendDatagram connSock $ A.encode response
                ) (\_ -> S.close connSock)

        -- add server handler to server map
        putMVar (nodeRt ^. RLens.servers)
          $ M.insert port (R.ServerHandle listenSockVar acceptWorkerId) servers

        putMVar registeredVar ()    -- server activity is now allowed

        pure $ Right ()
    )
    (\err -> do
      putMVar (nodeRt ^. RLens.servers) servers
      pure $ Left err
    )
