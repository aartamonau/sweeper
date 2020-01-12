module Player.SinglePoint
  ( player
  ) where

import Data.Ix (range)
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Ratio (Ratio, (%), denominator, numerator)
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO.Unsafe (unsafePerformIO)

import Game (Item(Empty, Mine), Game, Pos)
import qualified Game as Game
import Player.API (Player, PlayerL)
import qualified Player.API as API
import qualified Utils.Random as Random

data Move
  = OpenEmpty Pos
  | MarkMine Pos
  deriving (Eq, Ord)

findMoves :: Game -> [Pos] -> (Set Move, [Pos])
findMoves game = foldl' f z
  where
    z = (Set.empty, [])

    f (moves, ps) p
      | null thisMoves = (moves, p:ps)
      | otherwise      = (Set.union moves thisSet, ps)
      where
        thisMoves = posMoves game p
        thisSet   = Set.fromList thisMoves

posMoves :: Game -> Pos -> [Move]
posMoves game p
  | Just (Empty 0) <- item = []
  | Just (Empty c) <- item =
    if | numMines == c               -> map OpenEmpty unopened
       | numMines + numUnopened == c -> map MarkMine unopened
       | otherwise                   -> []
  | otherwise              = []
  where
    item = Game.item game p
    ns   = Game.neighbors game p

    (unopened, numMines) = foldl' f ([], 0) ns
      where
        f acc@(accUn, accMines) p =
          case Game.item game p of
            Nothing   -> (p:accUn, accMines)
            Just Mine -> (accUn, accMines+1)
            _         -> acc

    numUnopened = length unopened

player :: Player
player = API.makePlayer "single-point" strategy

strategy :: Pos -> PlayerL ()
strategy start = API.openEmpty start >>= loop

loop :: [Pos] -> PlayerL ()
loop opened = do
  game <- API.getGame
  let (moves, opened') = findMoves game opened

  newOpened <- if | Set.null moves -> playGreedy game opened'
                  | otherwise      -> loopMoves (Set.toList moves)

  loop (newOpened ++ opened')

loopMoves :: [Move] -> PlayerL [Pos]
loopMoves moves = concat <$> mapM playMove moves

playMove :: Move -> PlayerL [Pos]
playMove (OpenEmpty p) = do
  game <- API.getGame

  -- The square might have already gotten auto-opened as a side-effect of one
  -- of the preceding moves.
  if Game.isOpened game p
    then return []
    else API.openEmpty p
playMove (MarkMine p)  = API.markMine p >> return []

type Prob = Ratio Int
type Probs = [(Pos, Prob)]

posProbs :: Game -> Pos -> [(Pos, Prob)]
posProbs game p
  | Just (Empty 0) <- item = []
  | Just (Empty c) <- item =
      let prob = (c - numMines) % numUnopened
      in [(up, prob) | up <- unopened]
  | otherwise              = error "can't happen"
  where
    item = Game.item game p
    ns   = Game.neighbors game p

    unopened = filter (not . Game.isOpened game) ns
    mines    = [p | p <- ns, Just Mine == Game.item game p]

    numUnopened = length unopened
    numMines    = length mines

computeProbs :: Game -> [Pos] -> Probs
computeProbs game = Map.toList . foldl' f z . concatMap (posProbs game)
  where
    z = Map.empty
    f acc (p, prob) = Map.insertWith max p prob acc

playGreedy :: Game -> [Pos] -> PlayerL [Pos]
playGreedy game opened
  | null probs = playRandom game
  | otherwise  = do
    posInfo [(p, showProb prob) | (p, prob) <- probs]
    randomGreedyMove
  where
    probs = computeProbs game opened

    minProb = minimum (map snd probs)
    mins    = filter ((== minProb) . snd) probs

    randomGreedyMove = do
      let n = length mins
      i <- Random.getRandomR (0, n - 1)
      let (p, _) = mins !! i
      playMove (OpenEmpty p)

    showProb prob = show (numerator prob) ++ "/" ++ show (denominator prob)

posInfo :: [(Pos, String)] -> PlayerL ()
posInfo ps = pure $ unsafePerformIO $ print ps

playRandom :: Game -> PlayerL [Pos]
playRandom game = do
  i <- Random.getRandomR (0, n - 1)
  playMove (OpenEmpty $ unopened !! i)

  where
    bounds   = Game.bounds game
    unopened = filter (not . Game.isOpened game) (range bounds)
    n        = length unopened
