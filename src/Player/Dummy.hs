module Player.Dummy
  ( player
  ) where

import Game (Pos)
import qualified Game
import Player (Player, PlayerL)
import qualified Player as Player
import qualified Utils.Random as Random

player :: Player
player = Player.makePlayer "dummy" strategy

strategy :: Pos -> PlayerL ()
strategy start = do
  game <- Player.getGame
  let dims = (Game.numRows game, Game.numColumns game)

  _ <- Player.openEmpty start
  loop dims

loop :: (Int, Int) -> PlayerL ()
loop dims = randomMove dims >> loop dims

randomMove :: (Int, Int) -> PlayerL ()
randomMove (rows, columns) = do
  i <- Random.getRandomR (0, rows-1)
  j <- Random.getRandomR (0, columns-1)

  _ <- Player.openEmpty (i, j)

  return ()
