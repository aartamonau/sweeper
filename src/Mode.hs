module Mode
  ( parse
  ) where

import Options.Applicative (Parser, command, hsubparser, info, progDesc)

import Config (Config)

import Mode.Type (Mode(Mode))
import qualified Mode.Type as Mode

import qualified Mode.Bench
import qualified Mode.UI

parse :: Parser (Config -> IO ())
parse = hsubparser $ mconcat $ map handleMode modes
  where
    modes = [Mode.Bench.mode, Mode.UI.mode]
    handleMode (Mode {name, parse, help}) =
      command name $ info parse (progDesc help)
