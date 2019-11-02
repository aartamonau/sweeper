module Main
  ( main
  ) where

import CmdArgs (Mode(ModeUI, ModeBench), cfgMode, runWithCfg)
import qualified Mode.UI as UI
import qualified Mode.Bench as Bench

main :: IO ()
main = runWithCfg dispatch
  where
    dispatch cfg
      | ModeUI    uiCfg    <- mode = UI.run cfg uiCfg
      | ModeBench benchCfg <- mode = Bench.run cfg benchCfg
      where
        mode = cfgMode cfg
