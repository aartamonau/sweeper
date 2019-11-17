module Cli.Mode.Type
  ( Mode(Mode, name, help, parse)
  ) where

import Options.Applicative (Parser)

import Cli.Config (Config)

data Mode =
  Mode
    { name :: String
    , help :: String
    , parse :: Parser (Config -> IO ())
    }
