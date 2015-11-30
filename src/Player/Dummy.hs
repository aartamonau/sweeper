module Player.Dummy
       (
         newPlayer
       ) where

import System.Random (randomRIO)

import Game (Pos)
import Play (playRows, playColumns)
import Player.API (Player, Strategy,
                   makePlayer,
                   openEmpty, getPlay, io)

newPlayer :: Pos -> Player
newPlayer start = makePlayer "dummy" (newStrategy start)

newStrategy :: Pos -> Strategy ()
newStrategy start = do
  play <- getPlay
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
