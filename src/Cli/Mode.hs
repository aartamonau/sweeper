module Cli.Mode
  ( parse
  ) where

import Lens.Micro ((^.))
import Options.Applicative (Parser, command, hsubparser, info, progDesc)

import Cli.Config (Config)
-- import qualified Cli.Mode.Type as  (name, parse, help)
import qualified Cli.Mode.Type as Mode
import qualified Cli.Mode.Bench as Bench
import qualified Cli.Mode.UI as UI

parse :: Parser (Config -> IO ())
parse = hsubparser $ mconcat $ map handleMode modes
  where
    modes = [Bench.mode, UI.mode]
    handleMode mode =
      command (mode^.Mode.name) $ info (mode^.Mode.parse) (progDesc (mode^.Mode.help))
