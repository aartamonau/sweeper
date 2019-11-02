module Mode.Common
  ( randomGame
  ) where

import CmdArgs (Cfg, cfgBuffer, cfgFieldSpec, cfgStartMove)

import Game (Game)
import qualified Game as Game

import Rand (Gen, runRand)

randomGame :: Gen -> Cfg -> IO Game
randomGame gen cfg = runRand (Game.random rows cols mines startMove buffer) gen
  where
    (rows, cols, mines) = cfgFieldSpec cfg
    startMove           = cfgStartMove cfg
    buffer              = cfgBuffer cfg
