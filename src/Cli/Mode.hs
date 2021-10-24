module Cli.Mode (
    parse,
) where

import Options.Applicative (Parser, command, hsubparser, info, progDesc)

import Cli.Config (Config)
import qualified Cli.Mode.Bench as Bench
import Cli.Mode.Type (Mode (Mode))
import qualified Cli.Mode.Type as Mode
import qualified Cli.Mode.UI as UI

parse :: Parser (Config -> IO ())
parse = hsubparser $ mconcat $ map handleMode modes
  where
    modes = [Bench.mode, UI.mode]
    handleMode Mode{name, parse, help} =
        command name $ info parse (progDesc help)
