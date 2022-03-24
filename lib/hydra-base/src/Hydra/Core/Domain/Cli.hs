module Hydra.Core.Domain.Cli where

import           Hydra.Prelude
import           Hydra.Core.Domain.State (SignalVar)


data CliToken = CliToken
  { _finished :: SignalVar
  }


-- TODO: make it abstract for the client code
data CliAction
  = CliFinish (Maybe Text)
  | CliLoop
  | CliOutputMsg Text

cliFinishedToken :: CliToken -> SignalVar
cliFinishedToken token = _finished token
