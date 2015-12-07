module Main
       (
         main
       ) where

import CmdArgs (runWithCfg)
import qualified Mode.UI as UI

main :: IO ()
main = runWithCfg UI.run
