module Hydra.Framework.Cmd.Interpreter where

import           Hydra.Prelude

import qualified Data.Map as M
import qualified Data.Text as T

import qualified Hydra.Framework.Cmd.Language as L
import qualified Hydra.Core.Runtime           as R
import qualified Hydra.Core.Interpreters      as Impl
import qualified Hydra.Core.RLens             as RLens

putMsg :: IORef [String] -> Maybe String -> IO ()
putMsg msgsRef Nothing = pure ()
putMsg msgsRef (Just msg) = modifyIORef' msgsRef (msg:)

interpretCmdHandlerL :: R.CoreRuntime -> IORef [String] -> IORef Bool -> String -> L.CmdHandlerF a -> IO a

interpretCmdHandlerL coreRt msgsRef successRef line (L.CmdHandler tag method next) = do
  let tag' = takeWhile (/= ' ') line
  when (tag == tag') $ do
    mbMsg <- Impl.runLangL coreRt $ method line
    putMsg msgsRef mbMsg
  pure $ next ()

interpretCmdHandlerL coreRt msgsRef successRef line (L.UserCmd parser cont next) = do
  let verb = coreRt ^. RLens.cmdVerbosity
  next <$> case (parser line, verb) of
    (Left (L.ArgsNotParsed msg), R.WithArgErrors)  -> putMsg msgsRef $ Just msg
    (Left (L.ArgsNotParsed msg), R.WithSkipErrors) -> putMsg msgsRef $ Just msg
    (Left (L.SkipCmd       msg), R.WithSkipErrors) -> putMsg msgsRef $ Just msg
    (Left _, _)                                    -> pure ()
    (Right a, _) -> do
      mbMsg <- Impl.runLangL coreRt $ cont a
      putMsg msgsRef mbMsg
      writeIORef successRef True

runCmdHandlerL :: R.CoreRuntime -> IORef [String] -> IORef Bool -> String -> L.CmdHandlerL () -> IO ()
runCmdHandlerL coreRt msgsRef successRef line = foldFree (interpretCmdHandlerL coreRt msgsRef successRef line)
