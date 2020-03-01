module Hydra.Framework.Cmd.Interpreter where

import           Hydra.Prelude

import qualified Data.Map as M
import qualified Data.Text as T

import qualified Hydra.Framework.Cmd.Language as L
import qualified Hydra.Core.Runtime  as R
import qualified Hydra.Core.Interpreters  as Impl

interpretCmdHandlerL :: R.CoreRuntime -> String -> L.CmdHandlerF a -> IO a

interpretCmdHandlerL coreRt line (L.CmdHandler tag method next) = do
  let tag' = takeWhile (/= ' ') line
  next <$> if tag == tag'
    then do
      r <- Impl.runLangL coreRt $ method line
      print r
    else pure ()

interpretCmdHandlerL coreRt line (L.UserCmd parser cont next) = do
  print $ "Parsing a line: " ++ line
  next <$> case parser line of
    Left err -> print err $> ()
    Right a  -> Impl.runLangL coreRt $ cont a

runCmdHandlerL :: R.CoreRuntime -> String -> L.CmdHandlerL a -> IO a
runCmdHandlerL coreRt line = foldFree (interpretCmdHandlerL coreRt line)
