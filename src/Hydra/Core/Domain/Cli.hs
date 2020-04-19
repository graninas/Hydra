module Hydra.Core.Domain.Cli where

import           Hydra.Prelude
import           Hydra.Core.Domain.State (StateVar, SignalVar)


data CliToken = CliToken
  { _finished :: SignalVar
  }


-- TODO: make it abstract for the client code
data CliAction
  = CliFinish (Maybe String)
  | CliLoop
  | CliOutputMsg String

cliFinishedToken :: CliToken -> SignalVar
cliFinishedToken token = _finished token
