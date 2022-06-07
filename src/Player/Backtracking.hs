module Player.Backtracking
  ( player,
  )
where

import Control.Applicative ((<|>))
import Data.List (nub, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import Player (Item (Empty, Mine), Player, PlayerL, PlayerView, Pos)
import qualified Player
import Utils.Random (getRandomR)

player :: Player
player = Player.makePlayer "backtracking" strategy

strategy :: Pos -> PlayerL ()
strategy start = do
  ps <- Player.openEmpty start
  view <- Player.getPlayerView

  let frontier = unopenedNeighbors view ps
  let state = State {view, frontier}
  loop state

loop :: State -> PlayerL ()
loop state =
  getMove >>= \case
    Just (pos, move) -> do
      ps <- doMove pos move
      state' <- updateState pos ps state
      loop state'
    Nothing -> return ()
  where
    getMove =
      case findMove state of
        m@(Just _) -> return m
        Nothing -> do
          Player.debug "No moves found. Guessing."
          guessMove state

guessMove :: State -> PlayerL (Maybe (Pos, Move))
guessMove State {view, frontier} = go [corners, edges, border]
  where
    (_, (h, w)) = Player.bounds view
    corners = [(0, 0), (0, w), (h, 0), (h, w)]
    edges =
      concat $
        [[(0, i), (h, i)] | i <- [1 .. w - 1]]
          ++ [[(i, 0), (i, w)] | i <- [1 .. h - 1]]
    border = Set.toList frontier

    go [] = return Nothing
    go (ps : pps) =
      guessOne view ps >>= \case
        Nothing -> go pps
        Just pos -> return $ Just (pos, MoveEmpty)

guessOne :: PlayerView -> [Pos] -> PlayerL (Maybe Pos)
guessOne view ps =
  if
      | n == 0 ->
        return Nothing
      | otherwise -> do
        i <- getRandomR (0, n -1)
        return $ Just (unopened !! i)
  where
    unopened = filter (not . Player.isOpened view) ps
    n = length unopened

data Move
  = MoveMine
  | MoveEmpty
  deriving (Eq, Show)

flipMove :: Move -> Move
flipMove MoveMine = MoveEmpty
flipMove MoveEmpty = MoveMine

data State = State
  { view :: PlayerView,
    frontier :: Set Pos
  }

updateState :: Pos -> [Pos] -> State -> PlayerL State
updateState move opened State {frontier} = do
  view <- Player.getPlayerView
  let newUnopened = unopenedNeighbors view opened
  let frontier' = frontier \\ Set.fromList (move : opened)
  let frontier'' = frontier' `Set.union` newUnopened
  return $ State {view = view, frontier = frontier''}

doMove :: Pos -> Move -> PlayerL [Pos]
doMove p move =
  (p :)
    <$> case move of
      MoveMine -> Player.markMine p >> return []
      MoveEmpty -> Player.openEmpty p

-- TODO: make these parameters configurable
maxDepth :: Int
maxDepth = 3

maxNeighbors :: Int
maxNeighbors = 20

findMove :: State -> Maybe (Pos, Move)
findMove state@State {frontier} =
  case concatMap find [0 .. maxDepth] of
    [] -> Nothing
    ((pos, move) : _) -> Just (pos, flipMove move)
  where
    ps = Set.toList frontier
    moves = [(p, MoveMine) | p <- ps] ++ [(p, MoveEmpty) | p <- ps]
    isInfeasible depth = not . isFeasibleMove state depth
    find depth = filter (isInfeasible depth) moves

frontierNeighborsDepth :: State -> Int -> Pos -> [Pos]
frontierNeighborsDepth State {view, frontier} depth pos =
  tail $ go depth (Set.singleton pos) [pos]
  where
    expand ps seen =
      filter (not . (`Set.member` seen)) $
        concatMap (Player.neighbors view) ps

    go 0 _ acc = reverse $ filter (`Set.member` frontier) acc
    go i seen acc =
      let new = expand acc seen
          seen' = Set.union seen (Set.fromList new)
       in go (i - 1) seen' (new ++ acc)

isFeasibleMove :: State -> Int -> (Pos, Move) -> Bool
isFeasibleMove state depth posMove =
  assessMove state depth posMove False True (||)

assessMove ::
  State -> Int -> (Pos, Move) -> a -> a -> (a -> a -> a) -> a
assessMove state@State {view} depth (pos, move) z u f
  | isFeasibleAssignment view moves = checkNeighbors moves neighbors
  | otherwise = z
  where
    moves = Map.singleton pos move
    neighbors = take maxNeighbors $ frontierNeighborsDepth state depth pos

    checkNeighbors _ [] = u
    checkNeighbors moves (p : ps) =
      let mine = try p MoveMine moves ps
          empty = try p MoveEmpty moves ps
       in f mine empty

    try pos move moves rest =
      let moves' = Map.insert pos move moves
          feasible = isFeasibleAssignment view moves'
       in if feasible
            then checkNeighbors moves' rest
            else z

isFeasibleAssignment :: PlayerView -> Map Pos Move -> Bool
isFeasibleAssignment view moves =
  checkNumMinesTotal view moves && all (checkPosition view moves) neighbors
  where
    positions = Map.keys moves
    neighbors = usort $ concatMap (Player.neighbors view) positions
    usort = nub . sort

checkNumMinesTotal :: PlayerView -> Map Pos Move -> Bool
checkNumMinesTotal view moves
  | numMoves < numUnopened = mines <= minesTotal
  | otherwise = mines == minesTotal
  where
    minesTotal = Player.numMines view
    minesMarked = Player.numMinesMarked view
    minesTentative = Map.size $ Map.filter (== MoveMine) moves
    mines = minesMarked + minesTentative

    numMoves = Map.size moves
    numUnopened = Player.numUnopened view

checkPosition :: PlayerView -> Map Pos Move -> Pos -> Bool
checkPosition view moves pos =
  case Player.item view pos of
    Nothing -> True
    Just Mine -> True
    Just (Empty numMines) -> checkNumMines view pos numMines moves

checkNumMines :: PlayerView -> Pos -> Int -> Map Pos Move -> Bool
checkNumMines view pos numMines moves =
  numMarkedMines <= numMines && numUnopened + numMarkedMines >= numMines
  where
    getItem p = Player.item view p <|> convertMove <$> Map.lookup p moves

    convertMove MoveMine = Mine
    convertMove MoveEmpty = Empty 0

    neighbors = map getItem (Player.neighbors view pos)
    numMarkedMines = length $ filter (Just Mine ==) neighbors
    numUnopened = length $ filter (Nothing ==) neighbors

unopenedNeighbors :: PlayerView -> [Pos] -> Set Pos
unopenedNeighbors view = Set.fromList . filter isUnopened . concatMap neighbors
  where
    isUnopened = not . Player.isOpened view
    neighbors = Player.neighbors view
