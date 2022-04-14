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

-- TODO: rewrite this completely.

-- loopM :: Monad m => (a -> m (Either a b)) -> a -> m b
-- loopM act x = do
--   res <- act x
--   case res of
--     Left x -> loopM act x
--     Right v -> return v


-- -- TODO: what's about more than Word32?
-- readMsg :: S.Socket -> Either String Word32 -> IO [LByteString]
-- readMsg _    (Left  _  ) = pure []                      -- errors are swallowed?
-- readMsg sock (Right len) =
--   loopM (\(elemsOfMsg, restOfMsg) -> do
--     msg <- S.recv sock ((toEnum.fromEnum) restOfMsg)    -- what is this toEnum.fromEnum?
--     let newLen = len - toEnum (length msg)              -- what is this toEnum?
--     pure $ if newLen == 0
--       then Right $ msg : elemsOfMsg                     -- TODO: Lists? Is it efficient enough?
--       else Left (msg : elemsOfMsg, newLen)
--   ) ([], len)


readMsg :: S.Socket -> Int64 -> IO LByteString
readMsg sock len | len < 0  = pure "Socket reading error: datagram length is negative"
readMsg sock len | len == 0 = pure ""
readMsg sock len = do
  msg  <- S.recv sock len
  rest <- readMsg sock $ len - (LBS.length msg)
  pure $ msg <> rest


  -- loopM (\(elemsOfMsg, restOfMsg) -> do
  --   msg <- S.recv sock ((toEnum.fromEnum) restOfMsg)
  --   let newLen = len - toEnum (length msg)
  --   pure $ if newLen == 0
  --     then Right $ msg : elemsOfMsg
  --     else Left (msg : elemsOfMsg, newLen)
  -- ) ([], len)


msgSizeNumOfBytes = 4
maxDatagramSize = 4096

-- TODO: why lazy BS?
-- sendDatagram :: S.Socket -> LByteString -> IO ()
-- sendDatagram sock msg
--   = S.sendAll sock
--   $ encodeLazy (toEnum $ length msg :: Word32) <> msg       -- TODO: what's about more than Word32?

-- TODO: max datagram length check
sendDatagram :: S.Socket -> LByteString -> IO ()
sendDatagram sock msg = do
  S.sendAll sock $ encodeLazy (fromIntegral $ LBS.length msg :: Word32)
  S.sendAll sock msg
  -- print $ msg
  -- print $ encodeLazy (length msg)
  -- print $ encodeLazy (LBS.length msg)
  -- print $ encodeLazy (toEnum $ length msg :: Word32)
  -- print $ encodeLazy (toEnum $ length msg :: Word32) <> msg


-- receiveDatagram :: S.Socket -> IO LByteString
-- receiveDatagram sock = do
--   datagramLength <- mconcat <$> replicateM msgSizeNumOfBytes (S.recv sock 1)  -- TODO: why 4, why 1? Magic constants
--   rawMsg <- readMsg sock $ decodeLazy datagramLength
--   pure $ mconcat $ reverse rawMsg                             -- TODO: why reverse? Is it inefficient?

-- TODO: recv error checks
receiveDatagram :: S.Socket -> IO LByteString
receiveDatagram sock = do
  datagramLength <- S.recv sock msgSizeNumOfBytes
  print $ datagramLength

  case decodeLazy datagramLength of
    Left err   -> pure $ show err       -- TODO
    Right (len :: Word32) -> do
      rawMsg <- readMsg sock $ fromIntegral len
      print rawMsg
      pure rawMsg
