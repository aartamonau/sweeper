module Player.SinglePoint (
    player,
) where

import Data.Ix (range)
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Ratio (Ratio, (%))
import Data.Set (Set)
import qualified Data.Set as Set

import Player (Item (Empty, Mine), Player, PlayerL, PlayerView, Pos)
import qualified Player
import qualified Utils.Random as Random

data Move
    = OpenEmpty Pos
    | MarkMine Pos
    deriving (Eq, Ord)

findMoves :: PlayerView -> [Pos] -> (Set Move, [Pos])
findMoves view = foldl' f z
  where
    z = (Set.empty, [])

    f (moves, ps) p
        | null thisMoves = (moves, p : ps)
        | otherwise = (Set.union moves thisSet, ps)
      where
        thisMoves = posMoves view p
        thisSet = Set.fromList thisMoves

posMoves :: PlayerView -> Pos -> [Move]
posMoves view p
    | Just (Empty 0) <- item = []
    | Just (Empty c) <- item =
        if | numMines == c -> map OpenEmpty unopened
           | numMines + numUnopened == c -> map MarkMine unopened
           | otherwise -> []
    | otherwise = []
  where
    item = Player.item view p
    ns = Player.neighbors view p

    (unopened, numMines) = foldl' f ([], 0) ns
      where
        f acc@(accUn, accMines) p =
            case Player.item view p of
                Nothing -> (p : accUn, accMines)
                Just Mine -> (accUn, accMines + 1)
                _ -> acc

    numUnopened = length unopened

player :: Player
player = Player.makePlayer "single-point" strategy

strategy :: Pos -> PlayerL ()
strategy start = Player.openEmpty start >>= loop

loop :: [Pos] -> PlayerL ()
loop opened = do
    view <- Player.getPlayerView
    let (moves, opened') = findMoves view opened

    newOpened <-
        if | Set.null moves -> playGreedy view opened'
           | otherwise -> loopMoves (Set.toList moves)

    loop (newOpened ++ opened')

loopMoves :: [Move] -> PlayerL [Pos]
loopMoves moves = concat <$> mapM playMove moves

playMove :: Move -> PlayerL [Pos]
playMove (OpenEmpty p) = do
    view <- Player.getPlayerView

    -- The cell might have already gotten auto-opened as a side-effect of one of
    -- the preceding moves.
    if Player.isOpened view p
        then return []
        else Player.openEmpty p
playMove (MarkMine p) = Player.markMine p >> return []

type Prob = Ratio Int
type Probs = [(Pos, Prob)]

posProbs :: PlayerView -> Pos -> [(Pos, Prob)]
posProbs view p
    | Just (Empty 0) <- item = []
    | Just (Empty c) <- item =
        let prob = (c - numMines) % numUnopened
         in [(up, prob) | up <- unopened]
    | otherwise = error "can't happen"
  where
    item = Player.item view p
    ns = Player.neighbors view p

    unopened = filter (not . Player.isOpened view) ns
    mines = [p | p <- ns, Just Mine == Player.item view p]

    numUnopened = length unopened
    numMines = length mines

computeProbs :: PlayerView -> [Pos] -> Probs
computeProbs view = Map.toList . foldl' f z . concatMap (posProbs view)
  where
    z = Map.empty
    f acc (p, prob) = Map.insertWith max p prob acc

playGreedy :: PlayerView -> [Pos] -> PlayerL [Pos]
playGreedy view opened
    | null probs = playRandom view
    | otherwise = randomGreedyMove
  where
    probs = computeProbs view opened

    minProb = minimum (map snd probs)
    mins = filter ((== minProb) . snd) probs

    randomGreedyMove = do
        let n = length mins
        i <- Random.getRandomR (0, n - 1)
        let (p, _) = mins !! i
        playMove (OpenEmpty p)

playRandom :: PlayerView -> PlayerL [Pos]
playRandom view = do
    i <- Random.getRandomR (0, n - 1)
    playMove (OpenEmpty $ unopened !! i)
  where
    bounds = Player.bounds view
    unopened = filter (not . Player.isOpened view) (range bounds)
    n = length unopened
