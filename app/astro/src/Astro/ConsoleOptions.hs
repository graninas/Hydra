module Astro.ConsoleOptions
    ( ConsoleOptions (..)
    , Command (..)
    , ServerOptions (..)
    , ClientOptions (..)
    , RelDbOptions
    , parseConsoleOptions
    ) where

import           Hydra.Prelude
import qualified Prelude

import           Data.Semigroup ((<>))
import           Network.URI
import           Network.Wai.Handler.Warp (Port)
import           Options.Applicative

import           Astro.Client.Common        (ReportChannel(..), Approach(..))

data ConsoleOptions = ConsoleOptions Command deriving (Show)

data Command
    = Server ServerOptions
    | Client ClientOptions
      deriving (Show)

data ServerOptions = ServerOptions
    { soRelDbOptions :: RelDbOptions
    , soListenPort   :: Port
    } deriving (Show)

data RelDbOptions = UseSqliteDb String | UseMySqlDb URI

instance Show RelDbOptions where
    show (UseSqliteDb path) = path
    show (UseMySqlDb uri) = show uri

data ClientOptions = ClientOptions
    { coApproach :: Approach,
      coReportChannel :: ReportChannel
    } deriving (Show)

parseConsoleOptions :: IO ConsoleOptions
parseConsoleOptions
    = execParser $ info (consoleOptionParser <**> helper) fullDesc

consoleOptionParser =
    ConsoleOptions
    <$> hsubparser
            (  command "server"
                 (info serverOptionParser (progDesc "runs as server"))
            <> command "client"
                 (info clientOptionParser (progDesc "runs as client"))
            )

serverOptionParser :: Parser Command
serverOptionParser
    = Server <$> (ServerOptions
                  <$> relDbParser
                          <*> listenPortParser)
    where
      relDbParser = sqliteParser <|> mysqlParser
      sqliteParser = option filePathParser
                     (  long "sqlite"
                     <> metavar "DB"
                     <> help "path to sqlite file"
                     <> showDefault
                     <> value (UseSqliteDb "/tmp/astro.db")
                     )

      mysqlParser = option uriParser (  long "mysql"
                                     <> short 'u'
                                     <> metavar "URI"
                                     <> help "uri to database"
                                     <> showDefault
                                     <> value defaultUri
                                     )
      defaultUri = UseMySqlDb
                   $ fromJust (parseURI "mysql://root@localhost:3600/astro")

      listenPortParser = option auto (  long "listen-port"
                                     <> short 'l'
                                     <> metavar "PORT"
                                     <> help "port for serving HTTP requests"
                                     <> showDefault
                                     <> value 8080
                                     )

filePathParser = eitherReader parse
    where
      parse path = Right . UseSqliteDb $ path

uriParser = eitherReader parse
    where
      parse uri = maybe err (Right . UseMySqlDb) parsed
          where
            err = Left $ "Bad URI " ++ uri
            parsed = parseURI uri



clientOptionParser :: Parser Command
clientOptionParser
    = Client
      <$> (ClientOptions
              <$> option auto (  long "approach"
                      <> help ("approach one of: "
                               ++ show ([minBound .. maxBound] :: [Approach]))
                      <> showDefault
                      <> value SH
                      )
              <*> option channelParser
                      (  long "channel"
                      <> help "channel: http or tcp"
                      <> showDefault
                      <> value HttpChannel
                      ))

channelParser = eitherReader parse
    where
      parse "http" = Right HttpChannel
      parse "tcp" = Right TcpChannel
      parse o  = Left $ "Bad channel [" ++ o ++ "] must be [http] or [tcp]"
