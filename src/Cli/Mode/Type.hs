module Cli.Mode.Type
  ( Mode (Mode, name, help, parse),
  )
where

import Cli.Config (Config)
import Options.Applicative (Parser)

data Mode = Mode
  { name :: String,
    help :: String,
    parse :: Parser (Config -> IO ())
  }
