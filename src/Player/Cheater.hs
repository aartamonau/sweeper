module Player.Cheater
       (
         newPlayer
       ) where

import Data.Ix (range)
import Data.List ((\\))
import System.Random.Shuffle (shuffleM)

import Game (Game, Pos, Item(Empty, Mine), gameBounds, gameItem)
import Player.API (Player, Strategy,
                   makePlayer,
                   openEmpty, openMine, io)

newPlayer :: Game -> Pos -> Player
newPlayer game start = makePlayer "cheater" (newStrategy game start)

newStrategy :: Game -> Pos -> Strategy ()
newStrategy game start =
  do moves <- io $ shuffleM (range $ gameBounds game)

     let moves' = start : (moves \\ [start])
     loop game moves'

loop :: Game -> [Pos] -> Strategy ()
loop _     []    = return ()
loop game (p:ps) =
  case gameItem game p of
   Empty _ ->
     do opened <- openEmpty p
        loop game (ps \\ opened)
   Mine ->
     openMine p >> loop game ps
