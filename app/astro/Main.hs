{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import           Control.Monad
import           Hydra.Prelude
import qualified Hydra.Domain  as D
import qualified Hydra.Runtime as R
import qualified Hydra.Interpreters as R
import qualified Hydra.Language     as L

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
  lift $ lift $ R.runAppL rt flow


astroServer' :: AppServer
astroServer'
     = meteors
  :<|> meteor

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

main :: IO ()
main = do
  putStrLn ("Starting Astro App Server..." :: String)
  R.withAppRuntime (Just loggerCfg) $ \rt -> do
    appSt <- R.runAppL rt $ initState AppConfig
    run 8080 $ astroBackendApp $ Env rt appSt
