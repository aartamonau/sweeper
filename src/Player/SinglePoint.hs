{-# LANGUAGE MultiWayIf #-}

module Player.SinglePoint
       (
         player
       ) where

import Data.Ix (range)
import Data.List (foldl')
import Data.Ratio (Ratio, (%), numerator, denominator)

import qualified Data.Map.Strict as Map

import Data.Set (Set)
import qualified Data.Set as Set

import System.Random (randomRIO)

import Play (Play, Pos, Item(Empty, Mine),
             playItem, playNeighbors, playBounds, isOpened)
import Player.API (Player, Strategy,
                   makePlayer,
                   openEmpty, markMine, getPlay, posInfo, io)

data Move = OpenEmpty Pos
          | MarkMine Pos
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
         | numMines + numUnopened == c -> map MarkMine unopened
         | otherwise                   -> []
  | otherwise              = []
  where item     = playItem play p
        ns       = playNeighbors play p
        unopened = filter (not . isOpened play) ns
        mines    = [p | p <- ns,
                        Just Mine == playItem play p]

        numMines    = length mines
        numUnopened = length unopened

player :: Player
player = makePlayer "single-point" strategy

strategy :: Pos -> Strategy ()
strategy start = openEmpty start >>= loop

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
playMove (MarkMine p)  = markMine p >> return []

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

        unopened = filter (not . isOpened play) ns
        mines    = [p | p <- ns,
                        Just Mine == playItem play p]

        numUnopened = length unopened
        numMines    = length mines

computeProbs :: Play -> [Pos] -> Probs
computeProbs play = Map.toList . foldl' f z . concatMap (posProbs play)
  where z = Map.empty
        f acc (p, prob) = Map.insertWith max p prob acc

playGreedy :: Play -> [Pos] -> Strategy [Pos]
playGreedy play opened
  | null probs = playRandom play
  | otherwise  =
      do posInfo [(p, showProb prob) | (p, prob) <- probs]
         randomGreedyMove
  where probs = computeProbs play opened

        minProb = minimum (map snd probs)
        mins    = filter ((== minProb) . snd) probs

        randomGreedyMove =
          do i <- io $ randomRIO (0, n-1)
             let (p, _) = mins !! i
             playMove (OpenEmpty p)
          where n = length mins

        showProb prob = show (numerator prob) ++ "/" ++ show (denominator prob)

playRandom :: Play -> Strategy [Pos]
playRandom play =
  do i <- io $ randomRIO (0, n-1)
     playMove (OpenEmpty $ unopened !! i)

  where bounds   = playBounds play
        unopened = filter (not . isOpened play) (range bounds)
        n        = length unopened
