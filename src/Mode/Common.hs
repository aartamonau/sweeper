module Mode.Common
       (
         randomGame
       )
       where

import CmdArgs (Cfg, cfgFieldSpec, cfgStartMove, cfgBuffer)

import Game (Game)
import qualified Game as Game

randomGame :: Cfg -> IO Game
randomGame cfg = Game.randomGame rows cols mines startMove buffer
  where (rows, cols, mines) = cfgFieldSpec cfg
        startMove           = cfgStartMove cfg
        buffer              = cfgBuffer cfg
