module Main
       (
         main
       ) where

import System.Random (mkStdGen, setStdGen)

import CmdArgs (Mode(ModeUI, ModeBench), cfgMode, cfgSeed, runWithCfg)
import qualified Mode.UI as UI
import qualified Mode.Bench as Bench

main :: IO ()
main = runWithCfg $ \cfg ->
  do setSeed cfg
     dispatch cfg
  where dispatch cfg
          | ModeUI    uiCfg    <- mode = UI.run cfg uiCfg
          | ModeBench benchCfg <- mode = Bench.run cfg benchCfg
          | otherwise                  = error "can't happen"
          where mode = cfgMode cfg

        setSeed cfg
          | Just seed <- cfgSeed cfg = setStdGen (mkStdGen seed)
          | otherwise                = return ()
