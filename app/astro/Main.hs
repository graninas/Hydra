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
import qualified Astro.SqlDB.AstroDB as MDB


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
  :<|> EmptyAPI

astroAPI :: Proxy AstroAPI
astroAPI = Proxy

data Env = Env R.AppRuntime
type AppHandler = ReaderT Env (ExceptT ServerError IO)
type AppServer = ServerT AstroAPI (ReaderT Env (ExceptT ServerError IO))

astroServer' :: AppServer
astroServer' = meteors :<|> meteor :<|> emptyServer

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
  Env rt <- ask
  lift $ lift $ R.runAppL rt flow


data AppException
  = ConnectionFailedException Text
  | OperationFailedException Text
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, Exception)

doOrFail' :: Show e => (Text -> AppException) -> L.AppL (Either e a) -> L.AppL a
doOrFail' excF act = act >>= \case
  Left e  -> error $ show e
  Right a -> pure a

doOrFail :: Show e => L.AppL (Either e a) -> L.AppL a
doOrFail = doOrFail' OperationFailedException

connectOrFail :: D.DBConfig BS.SqliteM -> L.AppL (D.SqlConn BS.SqliteM)
connectOrFail cfg = doOrFail' ConnectionFailedException $ L.initSqlDB cfg

fromDBMeteor :: MDB.Meteor -> Meteor
fromDBMeteor MDB.Meteor {..} = Meteor
    _meteorId
    _meteorSize
    _meteorMass
    (Coords _meteorAzimuth _meteorAltitude)

getMeteors :: Maybe Int -> Maybe Int -> D.SqlConn BS.SqliteM -> L.AppL Meteors
getMeteors mbMass mbSize conn = do

  let predicate meteorDB = case (mbMass, mbSize) of
        (Just m, Just s)  -> (MDB._meteorSize meteorDB ==. B.val_ s)
            &&. (MDB._meteorMass meteorDB ==. B.val_ m)
        (Just m, Nothing) -> (MDB._meteorMass meteorDB ==. B.val_ m)
        (Nothing, Just s) -> (MDB._meteorSize meteorDB ==. B.val_ s)
        _                 -> B.val_ True

  eRows <- L.scenario
    $ L.runDB conn
    $ L.findRows
    $ B.select
    $ B.filter_ predicate
    $ B.all_ (MDB._meteors MDB.astroDb)
  case eRows of
    Right ms -> pure $ Meteors $ map fromDBMeteor ms
    Left err -> pure $ Meteors []

createMeteor :: MeteorTemplate -> D.SqlConn BS.SqliteM -> L.AppL MeteorID
createMeteor MeteorTemplate {..} conn = do
  doOrFail
    $ L.scenario
    $ L.runDB conn
    $ L.insertRows
    $ B.insert (MDB._meteors MDB.astroDb)
    $ B.insertExpressions
          [ MDB.Meteor B.default_
            (B.val_ size)
            (B.val_ mass)
            (B.val_ azimuth)
            (B.val_ altitude)
          ]

  let predicate meteorDB
          = (MDB._meteorSize meteorDB     ==. B.val_ size)
        &&. (MDB._meteorMass meteorDB     ==. B.val_ mass)
        &&. (MDB._meteorAzimuth meteorDB  ==. B.val_ azimuth)
        &&. (MDB._meteorAltitude meteorDB ==. B.val_ altitude)

  m <- doOrFail
    $ L.scenario
    $ L.runDB conn
    $ L.findRow
    $ B.select
    $ B.limit_ 1
    $ B.filter_ predicate
    $ B.all_ (MDB._meteors MDB.astroDb)
  pure $ MDB._meteorId $ fromJust m

dbConfig :: D.DBConfig BS.SqliteM
dbConfig = D.mkSQLiteConfig "/tmp/astro.db"

withDB
  :: D.DBConfig BS.SqliteM
  -> (D.SqlConn BS.SqliteM -> L.AppL a)
  -> L.AppL a
withDB cfg act = connectOrFail cfg >>= act

meteors :: Maybe Int -> Maybe Int -> AppHandler Meteors
meteors mbMass mbSize = runApp
  $ withDB dbConfig
  $ getMeteors mbMass mbSize

meteor :: MeteorTemplate -> AppHandler MeteorID
meteor meteor = runApp
  $ withDB dbConfig
  $ createMeteor meteor

loggerCfg :: D.LoggerConfig
loggerCfg = D.LoggerConfig
  { D._format       = "$prio $loggername: $msg"
  , D._level        = D.Debug
  , D._logFilePath  = ""
  , D._logToConsole = True
  , D._logToFile    = False
  }

main :: IO ()
main = R.withAppRuntime (Just loggerCfg)
  $ \rt -> run 8080 $ astroBackendApp $ Env rt
