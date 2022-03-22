module Hydra.Core.Networking.Internal.Socket where

import           Hydra.Prelude
import           Data.Serialize
import qualified Network.Socket.ByteString.Lazy                   as S
import qualified Network.Socket                                   as S hiding (recv, send)
-- import           Control.Monad.Extra

-- TODO: rewrite this completely.

loopM :: Monad m => (a -> m (Either a b)) -> a -> m b
loopM act x = do
  res <- act x
  case res of
    Left x -> loopM act x
    Right v -> return v


-- TODO: what's about more than Word32?
readMsg :: S.Socket -> Either String Word32 -> IO [LByteString]
readMsg _    (Left  _  ) = pure []                      -- errors are swallowed?
readMsg sock (Right len) =
  loopM (\(elemsOfMsg, restOfMsg) -> do
    msg <- S.recv sock ((toEnum.fromEnum) restOfMsg)    -- what is this toEnum.fromEnum?
    let newLen = len - toEnum (length msg)              -- what is this toEnum?
    pure $ if newLen == 0
      then Right $ msg : elemsOfMsg                     -- TODO: Lists? Is it efficient enough?
      else Left (msg : elemsOfMsg, newLen)
  ) ([], len)


-- TODO: why lazy BS?
sendDatagram :: S.Socket -> LByteString -> IO ()
sendDatagram sock msg
  = S.sendAll sock
  $ encodeLazy (toEnum $ length msg :: Word32) <> msg       -- TODO: what's about more than Word32?

receiveDatagram :: S.Socket -> IO LByteString
receiveDatagram sock = do
  datagramLength <- mconcat <$> replicateM 4 (S.recv sock 1)  -- TODO: why 4, why 1? Magic constants
  rawMsg <- readMsg sock $ decodeLazy datagramLength
  pure $ mconcat $ reverse rawMsg                             -- TODO: why reverse? Is it inefficient?
