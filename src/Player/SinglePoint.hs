{-# LANGUAGE MultiWayIf #-}

module Player.SinglePoint
       (
         newPlayer
       ) where

import Data.Ix (inRange, range)
import Data.List (foldl')
import Data.Maybe (isNothing)

import Data.Set (Set)
import qualified Data.Set as Set

import System.Random (randomRIO)

import Game (Pos, Item(Empty, Mine))
import Play (Play, playBounds, playItem)
import Player.API (Player, Strategy,
                   makePlayer,
                   openEmpty, openMine, getPlay, io)

data Move = OpenEmpty Pos
          | OpenMine Pos
          deriving (Eq, Ord)

findMoves :: Play -> [Pos] -> (Set Move, [Pos])
findMoves play = foldl' f z
  where z = (Set.empty, [])

        f (moves, ps) p | null thisMoves = (moves, p:ps)
                        | otherwise      = (Set.union moves thisSet, ps)
          where thisMoves = posMoves play p
                thisSet   = Set.fromList thisMoves

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

newPlayer :: Pos -> Player
newPlayer pos = makePlayer "single-point" (newStrategy pos)

newStrategy :: Pos -> Strategy ()
newStrategy start = openEmpty start >>= loop

loop :: [Pos] -> Strategy ()
loop opened =
  do play <- getPlay
     let (moves, opened') = findMoves play opened

     newOpened <- if | Set.null moves -> playRandom play
                     | otherwise      -> loopMoves (Set.toList moves)

     loop (newOpened ++ opened')

playRandom :: Play -> Strategy [Pos]
playRandom play =
  do r <- io $ randomRIO (0, n-1)
     openEmpty (unopened !! r)
  where bounds   = playBounds play
        unopened = [p | p <- range bounds, isNothing (playItem play p)]
        n        = length unopened

loopMoves :: [Move] -> Strategy [Pos]
loopMoves moves = concat <$> mapM play moves
  where play (OpenEmpty p) = openEmpty p
        play (OpenMine p)  = openMine p >> return []
