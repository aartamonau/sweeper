module Player.Dummy
  ( player
  ) where

import Game (Pos)
import qualified Game
import Player.API (Player, Strategy)
import qualified Player.API as API

player :: Player
player = API.makePlayer "dummy" strategy

strategy :: Pos -> Strategy ()
strategy start = do
  game <- API.getGame
  let dims = (Game.numRows game, Game.numColumns game)

  _ <- API.openEmpty start
  loop dims

loop :: (Int, Int) -> Strategy ()
loop dims = randomMove dims >> loop dims

randomMove :: (Int, Int) -> Strategy ()
randomMove (rows, columns) = do
  (i, j) <- API.rand $ (,) <$> API.uniformR (0, rows-1)
                           <*> API.uniformR (0, columns-1)

  _ <- API.openEmpty (i, j)

  return ()
