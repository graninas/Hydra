module Hydra.Core.Networking.Internal.Socket where

import           Hydra.Prelude
import           Data.Serialize
import qualified Network.Socket.ByteString.Lazy as S
import qualified Network.Socket                 as S hiding (recv, send)


import qualified Data.ByteString.Lazy as LBS

import qualified Data.ByteString as BS
import qualified Data.Serialize as DS
import qualified Network.Socket.ByteString.Lazy as SockLBS
import qualified Network.Socket.ByteString as SockBS
import qualified Network.Socket as Sock

import Debug.Trace (trace)

readMsg :: S.Socket -> Int -> IO ByteString
readMsg sock len | len < 0  = pure "Socket reading error: datagram length is negative"
readMsg sock len | len == 0 = pure ""
readMsg sock len = do
  msg  <- SockBS.recv sock len
  rest <- readMsg sock $ len - (BS.length msg)
  pure $ msg <> rest

msgSizeNumOfBytes :: Int
msgSizeNumOfBytes = 4       -- Word32
maxDatagramSize :: Int
maxDatagramSize = 4096
maxMsgSize :: Int
maxMsgSize = maxDatagramSize - msgSizeNumOfBytes

sendDatagram :: S.Socket -> ByteString -> IO ()
sendDatagram sock msg = do
  let l = BS.length msg
  when (l > maxMsgSize)
    $ error ("Message is too big: " <> show l <> ". Allowed: " <> show maxMsgSize)
  SockBS.sendAll sock $ DS.encode (fromIntegral l :: Word32)
  SockBS.sendAll sock msg

receiveDatagram :: S.Socket -> IO ByteString
receiveDatagram sock = do
  datagramLength <- SockBS.recv sock msgSizeNumOfBytes
  case DS.decode datagramLength of
    Left err   -> pure $ show err
    Right (len :: Word32) -> readMsg sock $ fromIntegral len
