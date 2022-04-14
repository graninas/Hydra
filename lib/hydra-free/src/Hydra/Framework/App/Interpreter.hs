module Hydra.Framework.App.Interpreter where

import           Hydra.Prelude

import qualified Hydra.Core.Domain        as D
import qualified Hydra.Core.Interpreters  as Impl
import qualified Hydra.Core.Language      as L
import qualified Hydra.Core.RLens         as RLens
import qualified Hydra.Runtime            as R
import qualified Hydra.Framework.Language as L
import qualified Hydra.Framework.RLens    as RLens
import qualified Hydra.Core.Networking.Internal.Socket as SockImpl

import qualified System.Console.Haskeline as HS

import           Data.Aeson as A
import qualified Data.Map as Map
import qualified Data.Text as T
import           Control.Concurrent (forkFinally)
import qualified Control.Exception.Safe as Safe
import qualified Network as N
import qualified Network.Socket as S hiding (recv)
import qualified Network.Socket.ByteString.Lazy as S

import qualified Data.ByteString.Lazy as LBS

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
  whenJust (T.unpack <$> mbMsg) HS.outputStrLn
  liftIO $ Impl.runLangL coreRt $ L.writeVarIO (D.cliFinishedToken cliToken) True
  pure True
evalCliAction _ _ D.CliLoop            = pure True
evalCliAction _ _ (D.CliOutputMsg msg) = HS.outputStrLn (T.unpack msg) >> pure True

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
  eRes <- startRpcServer appRt port protocol
  pure $ next eRes



interpretAppF appRt (L.CliF completeFunc onStep onUnknownCommand onParamsParseError methods cliToken next) = do
  let coreRt = appRt ^. RLens.coreRuntime

  handlersRef <- newIORef []
  Impl.runCliHandlerL handlersRef methods
  handlers <- readIORef handlersRef

  void $ forkIO $ do
    let loop = do

          -- TODO: configurable prompt
          mbLine <- HS.getInputLine "> "
          let mbLine' = (T.stripStart . T.stripEnd . T.pack) <$> mbLine

          let eHandlerResult = case mbLine' of
                Nothing   -> Left ()
                Just ""   -> Left ()
                Just line -> Right $ Impl.getHandler handlers line

          doLoop <- case eHandlerResult of
            Left ()    -> pure True
            Right (Impl.CmdHandlerNotFound line) -> do
              cliAction <- liftIO $ runAppL appRt $ onUnknownCommand line
              evalCliAction coreRt cliToken cliAction
            Right (Impl.CmdHandlerParamsError msg) -> do
              cliAction <- liftIO $ runAppL appRt $ onParamsParseError msg
              evalCliAction coreRt cliToken cliAction
            Right (Impl.CmdHandler action) -> do
              liftIO $ Impl.runLangL coreRt action
              cliAction <- liftIO $ runAppL appRt onStep
              evalCliAction coreRt cliToken cliAction

          when doLoop loop

    let completeFunc' = \str -> completeFunc $ T.pack str
    let cf = HS.completeWord Nothing " \t" $ pure . completeFunc'
    HS.runInputT (HS.setComplete cf HS.defaultSettings) loop

  pure $ next ()

runAppL :: R.AppRuntime -> L.AppL a -> IO a
runAppL appRt = foldFree (interpretAppF appRt)

instance R.StartApp L.AppL where
  startApp = runAppL



readRpcProtocol :: L.RpcProtocol () -> IO Impl.RpcHandlers
readRpcProtocol protocol = do
  handlersRef <- newIORef mempty
  void $ Impl.prepareRpcHandlers handlersRef protocol
  readIORef handlersRef


callRpc
  :: (L.LangL D.RpcResponse -> IO D.RpcResponse)
  -> Impl.RpcHandlers
  -> ByteString
  -> IO D.RpcResponse
callRpc runner handlers msg = case A.decodeStrict msg of
  Just (D.RpcRequest tag params reqId) -> case Map.lookup tag handlers of
    Just justMethod -> runner $ justMethod params reqId
    Nothing         -> pure $ D.RpcResponseError (A.String $ "The method " <> tag <> " isn't supported.") reqId
  Nothing -> pure $ D.RpcResponseError (A.String "error of request parsing") 0



startRpcServer
  :: R.AppRuntime
  -> D.Port
  -> L.RpcProtocol ()
  -> IO (Either D.RpcServerError ())
startRpcServer appRt port protocol = do
  handlers <- readRpcProtocol protocol

  let coreRt = appRt ^. RLens.coreRuntime
  servers  <- takeMVar $ coreRt ^. RLens.rpcServers

  -- TODO: rewrite this catchAny. Seems it doesn't clear resources in case of
  -- an exxception after N.listenOn
  eResult <- catchAny
    (do
        when (Map.member port servers)
          $ Safe.throwString $ "Port " <> show port <> " is used"

        registeredVar <- newEmptyMVar         -- no activity until the server is registered

        listenSock <- N.listenOn $ N.PortNumber $ fromIntegral port
        listenSockVar <- newMVar listenSock

        -- connection acceptance worker

        acceptWorkerId <- forkIO $ do
          takeMVar registeredVar              -- no activity until the server is registered

          forever $ do
            (connSock, _) <- S.accept listenSock

            -- rpc interaction worker
            -- TODO: track each worker and close on resource cleanup
            void $ forkFinally
                (do
                    msg      <- SockImpl.receiveDatagram connSock
                    response <- callRpc (Impl.runLangL coreRt) handlers msg
                    SockImpl.sendDatagram connSock $ LBS.toStrict $ A.encode response
                ) (\_ -> S.close connSock)

        -- add server handler to server map
        putMVar (coreRt ^. RLens.rpcServers)
          $ Map.insert port (R.ServerHandle listenSockVar acceptWorkerId) servers

        putMVar registeredVar ()    -- server activity is now allowed

        pure $ Right ()
    )
    (\err -> do
      putMVar (coreRt ^. RLens.rpcServers) servers
      pure $ Left err
    )

  case eResult of
    Left (err :: SomeException) -> pure $ Left $ show err
    Right () -> pure $ Right ()
