module Cli.Mode.Common
  ( randomGame
  ) where

import Cli.Config (Config)
import qualified Cli.Config as Config

import Game (Game)
import qualified Game as Game

import Rand (MonadRandom)

randomGame :: MonadRandom m => Config -> m Game
randomGame cfg = Game.random rows cols mines startMove buffer
  where
    (rows, cols, mines) = Config.fieldSpec cfg
    startMove           = Config.startMove cfg
    buffer              = Config.buffer cfg
