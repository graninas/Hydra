{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}

module Astro.Server
  ( runAstroServer
  ) where

import           Control.Monad
import           System.Process (readCreateProcess, shell)
import           Network.Wai.Handler.Warp (run)
import           Servant
import           Data.Time
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import           Data.Time.Clock (UTCTime)
import qualified Database.Beam as B
import qualified Database.Beam.Sqlite as BS
import qualified Database.Beam.Backend.SQL as B
import           Database.Beam ((==.), (&&.), (<-.), (/=.), (==?.))

import           Hydra.Prelude
import qualified Hydra.Domain  as D
import qualified Hydra.Runtime as R
import qualified Hydra.Interpreters as R
import qualified Hydra.Language     as L

import           Astro.Domain.Meteor
import           Astro.Catalogue
import           Astro.Types

type AstroAPI
  = (  "meteors"
    :> QueryParam "mass" Int
    :> QueryParam "size" Int
    :> Get '[JSON] Meteors
    )
  :<|>
    (  "meteor"
    :> ReqBody '[JSON] MeteorTemplate
    :> Post '[JSON] MeteorID
    )

astroAPI :: Proxy AstroAPI
astroAPI = Proxy

data Env = Env !R.AppRuntime !AppState
type AppHandler = ReaderT Env (ExceptT ServerError IO)
type AppServer = ServerT AstroAPI AppHandler

astroServer :: Env -> Server AstroAPI
astroServer env = hoistServer astroAPI (f env) astroServer'
  where
    f :: Env -> ReaderT Env (ExceptT ServerError IO) a -> Handler a
    f env r = do
      eResult <- liftIO $ (runExceptT $ runReaderT r env )
      case eResult of
        Left err  -> throwError err
        Right res -> pure res

astroBackendApp :: Env -> Application
astroBackendApp = serve astroAPI . astroServer

runApp :: L.AppL a -> AppHandler a
runApp flow = do
  Env rt _ <- ask
  eRes <- lift $ lift $ try $ R.runAppL rt flow
  case eRes of
    Left (err :: SomeException) -> do
      liftIO $ putStrLn @String $ "Exception handled: " <> show err
      throwError err500
    Right res -> pure res


astroServer' :: AppServer
astroServer'
     = meteors
  :<|> meteor

-- TODO: configs from the command line
dbConfig :: D.DBConfig BS.SqliteM
dbConfig = D.mkSQLiteConfig "/tmp/astro.db"

meteors :: Maybe Int -> Maybe Int -> AppHandler Meteors
meteors mbMass mbSize = runApp
  $ withDB dbConfig
  $ getMeteors mbMass mbSize

meteor :: MeteorTemplate -> AppHandler MeteorID
meteor m = runApp
  $ withDB dbConfig
  $ createMeteor m

loggerCfg :: D.LoggerConfig
loggerCfg = D.LoggerConfig
  { D._format       = "$prio $loggername: $msg"
  , D._level        = D.Debug
  , D._logFilePath  = ""
  , D._logToConsole = True
  , D._logToFile    = False
  }

prepareSQLiteDB :: IO ()
prepareSQLiteDB = do
  putStrLn @String "Copying astro_template.db to /tmp/astro.db..."
  eRes <- try $ void $ readCreateProcess (shell "cp ./app/astro/astro_template.db /tmp/astro.db") ""
  case eRes of
    Left (err :: SomeException) -> do
      putStrLn @String $ "Exception got: " <> show err
      error $ "Exception got: " <> show err
    Right _ -> pure ()

runAstroServer :: IO ()
runAstroServer = do
  prepareSQLiteDB
  putStrLn @String "Starting Astro App Server..."
  R.withAppRuntime (Just loggerCfg) $ \rt -> do
    appSt <- R.runAppL rt $ initState AppConfig
    run 8080 $ astroBackendApp $ Env rt appSt
