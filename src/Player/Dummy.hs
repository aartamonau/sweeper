module Player.Dummy
       (
         player
       ) where

import System.Random (randomRIO)

import Play (Pos, playRows, playColumns)
import Player.API (Player, Strategy,
                   makePlayer,
                   openEmpty, getPlay, io)

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
  do i <- io $ randomRIO (0, rows-1)
     j <- io $ randomRIO (0, columns-1)

     _ <- openEmpty (i, j)

     return ()
