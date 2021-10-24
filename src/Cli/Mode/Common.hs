module Cli.Mode.Common (
    randomGame,
) where

import Cli.Config (Config)
import qualified Cli.Config as Config
import Game (Game)
import qualified Game
import Utils.Random (MonadRandom, StdGen)
import qualified Utils.Random as Random

randomGame :: StdGen -> Config -> Game
randomGame gen = flip Random.evalRand gen . randomGame'

randomGame' :: MonadRandom m => Config -> m Game
randomGame' cfg = Game.random rows cols mines startMove buffer
  where
    (rows, cols, mines) = Config.fieldSpec cfg
    startMove = Config.startMove cfg
    buffer = Config.buffer cfg
