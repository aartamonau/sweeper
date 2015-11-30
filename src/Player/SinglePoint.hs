{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}

module Player.SinglePoint
       (
         newPlayer
       ) where

import Data.List (foldl')
import Data.Maybe (isNothing)
import Data.Ratio (Ratio, (%), numerator, denominator)

import qualified Data.Map.Strict as Map

import Data.Set (Set)
import qualified Data.Set as Set

import System.Random (randomRIO)

import Colors (black)
import Draw (Draw, drawText, setStrokeColor, setFillColor, setFont)
import Game (Pos, Item(Empty, Mine))
import Play (Play, playItem, playNeighbors)
import Player.API (Player, Strategy,
                   makePlayer,
                   openEmpty, openMine, getPlay, draw, io)

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
        ns       = playNeighbors play p
        unopened = filter (isNothing . playItem play) ns
        mines    = [p | p <- ns,
                        Just Mine == playItem play p]

        numMines    = length mines
        numUnopened = length unopened

newPlayer :: Pos -> Player
newPlayer pos = makePlayer "single-point" (newStrategy pos)

newStrategy :: Pos -> Strategy ()
newStrategy start = openEmpty start >>= loop

loop :: [Pos] -> Strategy ()
loop opened =
  do play <- getPlay
     let (moves, opened') = findMoves play opened

     newOpened <- if | Set.null moves -> playGreedy play opened'
                     | otherwise      -> loopMoves (Set.toList moves)

     loop (newOpened ++ opened')

loopMoves :: [Move] -> Strategy [Pos]
loopMoves moves = concat <$> mapM playMove moves

playMove :: Move -> Strategy [Pos]
playMove (OpenEmpty p) = openEmpty p
playMove (OpenMine p)  = openMine p >> return []

type Prob  = Ratio Int
type Probs = [(Pos, Prob)]

posProbs :: Play -> Pos -> [(Pos, Prob)]
posProbs play p
  | Just (Empty 0) <- item = []
  | Just (Empty c) <- item =
      let prob = (c - numMines) % numUnopened
      in [(up, prob) | up <- unopened]
  | otherwise              = error "can't happen"
  where item = playItem play p
        ns   = playNeighbors play p

        unopened = filter (isNothing . playItem play) ns
        mines    = [p | p <- ns,
                        Just Mine == playItem play p]

        numUnopened = length unopened
        numMines    = length mines

computeProbs :: Play -> [Pos] -> Probs
computeProbs play = Map.toList . foldl' f z . concatMap (posProbs play)
  where z = Map.empty
        f acc (p, prob) = Map.insertWith max p prob acc

playGreedy :: Play -> [Pos] -> Strategy [Pos]
playGreedy play opened =
  do draw [(p, drawProb prob) | (p, prob) <- probs]
     doMove
  where probs = computeProbs play opened

        minProb = minimum (map snd probs)
        maxProb = maximum (map snd probs)

        maxs = filter ((== maxProb) . snd) probs
        mins = filter ((== minProb) . snd) probs

        randomMove f xs =
          do i <- io $ randomRIO (0, n-1)
             let (p, _) = xs !! i
             playMove (f p)
          where n = length xs

        doMove | maxProb > (1 - minProb) = randomMove OpenMine maxs
               | otherwise               = randomMove OpenEmpty mins

drawProb :: Prob -> Draw ()
drawProb prob =
  do let num   = numerator prob
     let denom = denominator prob
     let text  = show num ++ "/" ++ show denom
     setFont "monospace" 0.4
     setFillColor black
     setStrokeColor black
     drawText text (0.5,0.5)
