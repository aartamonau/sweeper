module Player.Dummy
       (
         player
       ) where

import Play (Pos, playRows, playColumns)
import Player.API (Player, Strategy,
                   makePlayer,
                   openEmpty, getPlay,
                   rand, uniformR)

player :: Player
player = makePlayer "dummy" strategy

strategy :: Pos -> Strategy ()
strategy start =
  do play <- getPlay
     let dims = (playRows play, playColumns play)

     _ <- openEmpty start
     loop dims

loop :: (Int, Int) -> Strategy ()
loop dims = randomMove dims >> loop dims

randomMove :: (Int, Int) -> Strategy ()
randomMove (rows, columns) =
  do (i, j) <- rand $ (,) <$> uniformR (0, rows-1)
                          <*> uniformR (0, columns-1)

     _ <- openEmpty (i, j)

     return ()
