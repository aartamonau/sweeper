{-# LANGUAGE MultiWayIf #-}

module Player.SinglePoint
       where

import Data.Ix (inRange, range)
import Data.List (foldl')
import Data.Maybe (isNothing)

import Data.Set (Set)
import qualified Data.Set as Set

import System.Random (randomRIO)

import Game (Pos, Item(Empty, Mine))
import Play (Play, playBounds, playItem)
import Player.API (Player, openEmpty, openMine, getPlay, io)

data Move = OpenEmpty Pos
          | OpenMine Pos
          deriving (Eq, Ord)

findMoves :: Play -> Set Move
findMoves play =
  foldl' Set.union Set.empty $ map (posMovesSet play) (range bounds)
  where bounds = playBounds play

posMovesSet :: Play -> Pos -> Set Move
posMovesSet play = Set.fromList . posMoves play

posMoves :: Play -> Pos -> [Move]
posMoves play p
  | Just (Empty 0) <- item = []
  | Just (Empty c) <- item =
      if | numMines == c               -> map OpenEmpty unopened
         | numMines + numUnopened == c -> map OpenMine unopened
         | otherwise                   -> []
  | otherwise              = []
  where item     = playItem play p
        ns       = neighbors play p
        unopened = filter (isNothing . playItem play) ns
        mines    = [p | p <- ns,
                        Just Mine == playItem play p]

        numMines    = length mines
        numUnopened = length unopened

neighbors :: Play -> Pos -> [Pos]
neighbors play p@(pi, pj) =
  [q | di <- [-1, 0, 1], dj <- [-1, 0, 1],
       let q = (pi + di, pj + dj),
       q /= p, inRange bounds q]

  where bounds = playBounds play

newPlayer :: Pos -> Player ()
newPlayer start = openEmpty start >> loop

loop :: Player ()
loop =
  do play <- getPlay
     let moves = findMoves play

     if | Set.null moves -> playRandom play
        | otherwise      -> loopMoves (Set.toList moves)

     loop

playRandom :: Play -> Player ()
playRandom play =
  do r <- io $ randomRIO (0, n-1)
     _ <- openEmpty (unopened !! r)

     return ()
  where bounds   = playBounds play
        unopened = [p | p <- range bounds, isNothing (playItem play p)]
        n        = length unopened

loopMoves :: [Move] -> Player ()
loopMoves = mapM_ play
  where play (OpenEmpty p) = openEmpty p >> return ()
        play (OpenMine p)  = openMine p
