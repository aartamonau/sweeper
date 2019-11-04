module Main
  ( main
  ) where

import CmdArgs (Mode(ModeUI, ModeBench))
import qualified CmdArgs

import qualified Mode.UI as UI
import qualified Mode.Bench as Bench

main :: IO ()
main = CmdArgs.run dispatch
  where
    dispatch cfg (ModeUI uiCfg)       = UI.run cfg uiCfg
    dispatch cfg (ModeBench benchCfg) = Bench.run cfg benchCfg
