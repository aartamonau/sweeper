module DummyPlayer
       (
         newPlayer
       ) where

import System.Random (randomRIO)

import Game (Pos)
import Play (playRows, playColumns)
import PlayerAPI (Player, openEmpty, getPlay, io)

newPlayer :: Pos -> Player ()
newPlayer startHint = do
  play <- getPlay
  let dims = (playRows play, playColumns play)

  _ <- openEmpty startHint
  loop dims

loop :: (Int, Int) -> Player ()
loop dims = randomMove dims >> loop dims

randomMove :: (Int, Int) -> Player ()
randomMove (rows, columns) =
  do i <- io $ randomRIO (0, rows-1)
     j <- io $ randomRIO (0, columns-1)

     _ <- openEmpty (i, j)

     return ()
