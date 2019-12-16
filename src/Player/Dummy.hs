module Player.Dummy
  ( player
  ) where

import Game (Pos)
import qualified Game
import Player.API (Player, PlayerL)
import qualified Player.API as API
import qualified Rand as Rand

player :: Player
player = API.makePlayer "dummy" strategy

strategy :: Pos -> PlayerL ()
strategy start = do
  game <- API.getGame
  let dims = (Game.numRows game, Game.numColumns game)

  _ <- API.openEmpty start
  loop dims

loop :: (Int, Int) -> PlayerL ()
loop dims = randomMove dims >> loop dims

randomMove :: (Int, Int) -> PlayerL ()
randomMove (rows, columns) = do
  i <- API.rand $ Rand.uniformR (0, rows-1)
  j <- API.rand $ Rand.uniformR (0, columns-1)

  _ <- API.openEmpty (i, j)

  return ()
