{-# LANGUAGE MultiWayIf #-}

module Player.SinglePoint
  ( player
  ) where

import Data.Ix (range)
import Data.List (foldl')
import Data.Ratio (Ratio, (%), denominator, numerator)

import qualified Data.Map.Strict as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Play (Item(Empty, Mine), Play, Pos)
import qualified Play

import Player.API
  ( Player
  , Strategy
  , getPlay
  , makePlayer
  , markMine
  , openEmpty
  , posInfo
  , rand
  , uniformR
  )

data Move
  = OpenEmpty Pos
  | MarkMine Pos
  deriving (Eq, Ord)

findMoves :: Play -> [Pos] -> (Set Move, [Pos])
findMoves play = foldl' f z
  where
    z = (Set.empty, [])

    f (moves, ps) p
      | null thisMoves = (moves, p:ps)
      | otherwise      = (Set.union moves thisSet, ps)
      where
        thisMoves = posMoves play p
        thisSet   = Set.fromList thisMoves

posMoves :: Play -> Pos -> [Move]
posMoves play p
  | Just (Empty 0) <- item = []
  | Just (Empty c) <- item =
    if | numMines == c               -> map OpenEmpty unopened
       | numMines + numUnopened == c -> map MarkMine unopened
       | otherwise                   -> []
  | otherwise              = []
  where
    item = Play.item play p
    ns   = Play.neighbors play p

    (unopened, numMines) = foldl' f ([], 0) ns
      where
        f acc@(accUn, accMines) p =
          case Play.item play p of
            Nothing   -> (p:accUn, accMines)
            Just Mine -> (accUn, accMines+1)
            _         -> acc

    numUnopened = length unopened

player :: Player
player = makePlayer "single-point" strategy

strategy :: Pos -> Strategy ()
strategy start = openEmpty start >>= loop

loop :: [Pos] -> Strategy ()
loop opened = do
  play <- getPlay
  let (moves, opened') = findMoves play opened

  newOpened <- if | Set.null moves -> playGreedy play opened'
                  | otherwise      -> loopMoves (Set.toList moves)

  loop (newOpened ++ opened')

loopMoves :: [Move] -> Strategy [Pos]
loopMoves moves = concat <$> mapM playMove moves

playMove :: Move -> Strategy [Pos]
playMove (OpenEmpty p) = openEmpty p
playMove (MarkMine p)  = markMine p >> return []

type Prob = Ratio Int
type Probs = [(Pos, Prob)]

posProbs :: Play -> Pos -> [(Pos, Prob)]
posProbs play p
  | Just (Empty 0) <- item = []
  | Just (Empty c) <- item =
      let prob = (c - numMines) % numUnopened
      in [(up, prob) | up <- unopened]
  | otherwise              = error "can't happen"
  where
    item = Play.item play p
    ns   = Play.neighbors play p

    unopened = filter (not . Play.isOpened play) ns
    mines    = [p | p <- ns, Just Mine == Play.item play p]

    numUnopened = length unopened
    numMines    = length mines

computeProbs :: Play -> [Pos] -> Probs
computeProbs play = Map.toList . foldl' f z . concatMap (posProbs play)
  where
    z = Map.empty
    f acc (p, prob) = Map.insertWith max p prob acc

playGreedy :: Play -> [Pos] -> Strategy [Pos]
playGreedy play opened
  | null probs = playRandom play
  | otherwise  = do
    posInfo [(p, showProb prob) | (p, prob) <- probs]
    randomGreedyMove
  where
    probs = computeProbs play opened

    minProb = minimum (map snd probs)
    mins    = filter ((== minProb) . snd) probs

    randomGreedyMove = do
      let n = length mins
      i <- rand $ uniformR (0, n - 1)
      let (p, _) = mins !! i
      playMove (OpenEmpty p)

    showProb prob = show (numerator prob) ++ "/" ++ show (denominator prob)

playRandom :: Play -> Strategy [Pos]
playRandom play = do
  i <- rand $ uniformR (0, n - 1)
  playMove (OpenEmpty $ unopened !! i)

  where
    bounds   = Play.bounds play
    unopened = filter (not . Play.isOpened play) (range bounds)
    n        = length unopened
