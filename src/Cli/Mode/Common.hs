module Cli.Mode.Common
  ( randomGame
  , randomGameIO
  ) where

import Control.Monad.Random.Class (MonadRandom)
import Control.Monad.Random.Strict (evalRand)
import System.Random (StdGen)

import Cli.Config (Config)
import qualified Cli.Config as Config

import Game (Game)
import qualified Game as Game

randomGameIO :: Config -> IO Game
randomGameIO = randomGame'

randomGame :: StdGen -> Config -> Game
randomGame gen = flip evalRand gen . randomGame'

randomGame' :: MonadRandom m => Config -> m Game
randomGame' cfg = Game.random rows cols mines startMove buffer
  where
    (rows, cols, mines) = Config.fieldSpec cfg
    startMove           = Config.startMove cfg
    buffer              = Config.buffer cfg
