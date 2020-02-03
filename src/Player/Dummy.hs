module Player.Dummy
  ( player
  ) where

import Player (Player, PlayerL, Pos)
import qualified Player as Player
import qualified Utils.Random as Random

player :: Player
player = Player.makePlayer "dummy" strategy

strategy :: Pos -> PlayerL ()
strategy start = do
  view <- Player.getPlayerView
  let dims = (Player.numRows view, Player.numColumns view)

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
