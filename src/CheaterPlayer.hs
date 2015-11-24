module CheaterPlayer
       (
         newPlayer
       ) where

import Data.Ix (range)
import Data.List ((\\))
import System.Random.Shuffle (shuffleM)

import Game (Game, Pos, Item(Empty, Mine), gameBounds, gameItem)
import PlayerAPI (Player, openEmpty, openMine, io)

newPlayer :: Game -> Pos -> Player ()
newPlayer game start =
  do moves <- io $ shuffleM (range $ gameBounds game)

     let moves' = start : (moves \\ [start])
     loop game moves'

loop :: Game -> [Pos] -> Player ()
loop _     []    = return ()
loop game (p:ps) =
  case gameItem game p of
   Empty _ ->
     do opened <- openEmpty p
        loop game (ps \\ opened)
   Mine ->
     openMine p >> loop game ps
