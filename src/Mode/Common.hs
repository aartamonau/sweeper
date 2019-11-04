module Mode.Common
  ( randomGame
  ) where

import Config (Config)
import qualified Config

import Game (Game)
import qualified Game as Game

import Rand (Gen, runRand)

randomGame :: Gen -> Config -> IO Game
randomGame gen cfg = runRand (Game.random rows cols mines startMove buffer) gen
  where
    (rows, cols, mines) = Config.fieldSpec cfg
    startMove           = Config.startMove cfg
    buffer              = Config.buffer cfg
