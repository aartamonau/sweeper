module Player.Dummy
  ( player
  ) where

import Game (Pos)
import qualified Game
import Player.API
  ( Player
  , Strategy
  , getGame
  , makePlayer
  , openEmpty
  , rand
  , uniformR
  )

player :: Player
player = makePlayer "dummy" strategy

strategy :: Pos -> Strategy ()
strategy start = do
  game <- getGame
  let dims = (Game.rows game, Game.columns game)

  _ <- openEmpty start
  loop dims

loop :: (Int, Int) -> Strategy ()
loop dims = randomMove dims >> loop dims

randomMove :: (Int, Int) -> Strategy ()
randomMove (rows, columns) = do
  (i, j) <- rand $ (,) <$> uniformR (0, rows-1)
                       <*> uniformR (0, columns-1)

  _ <- openEmpty (i, j)

  return ()
