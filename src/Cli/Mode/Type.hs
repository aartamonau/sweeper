module Cli.Mode.Type
  ( Mode(Mode, _name, _help, _parse)
  , name
  , help
  , parse
  ) where

import Lens.Micro.TH (makeLenses)
import Options.Applicative (Parser)

import Cli.Config (Config)

data Mode =
  Mode
    { _name :: String
    , _help :: String
    , _parse :: Parser (Config -> IO ())
    }
makeLenses ''Mode
