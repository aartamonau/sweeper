module Main
       (
         main
       ) where

import CmdArgs (Mode(ModeUI), cfgMode, runWithCfg)
import qualified Mode.UI as UI

main :: IO ()
main = runWithCfg dispatch
  where dispatch cfg
          | ModeUI uiCfg <- mode = UI.run cfg uiCfg
          where mode = cfgMode cfg
