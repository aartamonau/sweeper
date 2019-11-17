module Mode.Type
  ( Mode(Mode, name, help, parse)
  ) where

import Options.Applicative (Parser)

import Config (Config)

data Mode =
  Mode
    { name :: String
    , help :: String
    , parse :: Parser (Config -> IO ())
    }
