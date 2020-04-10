module Player.Backtracking
  ( player
  ) where

import Control.Applicative ((<|>))
import Data.List (sort, nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import Player (Item(Mine, Empty), Player, PlayerL, PlayerView, Pos)
import qualified Player as Player

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
  case findMove state of
    Just (pos, move) -> do
      ps <- doMove pos move
      state' <- updateState pos ps state
      loop state'
    Nothing -> return ()

data Move
  = MoveMine
  | MoveEmpty
  deriving Show

flipMove :: Move -> Move
flipMove MoveMine = MoveEmpty
flipMove MoveEmpty = MoveMine

data State =
  State
    { view :: PlayerView
    , frontier :: Set Pos
    }

updateState :: Pos -> [Pos] -> State -> PlayerL State
updateState move opened (State {frontier}) = do
  view <- Player.getPlayerView
  let newUnopened = unopenedNeighbors view opened
  let frontier' = frontier \\ Set.fromList (move : opened)
  let frontier'' = frontier' `Set.union` newUnopened
  return $ State {view = view, frontier = frontier''}

doMove :: Pos -> Move -> PlayerL [Pos]
doMove p move =
  case move of
    MoveMine -> Player.markMine p >> return []
    MoveEmpty -> Player.openEmpty p

findMove :: State -> Maybe (Pos, Move)
findMove state@(State {frontier}) =
  case concatMap find [0..2] of
    [] -> Nothing
    ((pos, move):_) -> Just (pos, flipMove move)
  where
    ps = Set.toList frontier
    moves = [(p, MoveMine) | p <- ps] ++ [(p, MoveEmpty) | p <- ps]
    isInfeasible depth = not . uncurry (isFeasibleMove state depth)
    find depth = filter (isInfeasible depth) moves

frontierNeighborsDepth :: State -> Int -> Pos -> [Pos]
frontierNeighborsDepth state depth pos =
  tail $ go depth (Set.singleton pos) [pos]
  where
    expand ps seen =
      filter (not . (`Set.member` seen)) $
      concatMap (frontierNeighbors state) ps

    go 0 _ acc = reverse acc
    go i seen acc =
      let new = expand acc seen
          seen' = Set.union seen (Set.fromList new)
       in go (i - 1) seen' (new ++ acc)

frontierNeighbors :: State -> Pos -> [Pos]
frontierNeighbors (State {view, frontier}) pos =
  filter (`Set.member` frontier) $ Player.neighbors view pos

isFeasibleMove :: State -> Int -> Pos -> Move -> Bool
isFeasibleMove state@(State {view}) depth pos move =
  isFeasibleAssignment view moves && checkNeighbors moves neighbors
  where
    moves = Map.singleton pos move
    neighbors = frontierNeighborsDepth state depth pos

    checkNeighbors _ [] = True
    checkNeighbors moves (p:ps) =
      try p MoveMine moves ps || try p MoveEmpty moves ps

    try pos move moves rest =
      let moves' = Map.insert pos move moves
       in isFeasibleAssignment view moves' && checkNeighbors moves' rest

isFeasibleAssignment :: PlayerView -> Map Pos Move -> Bool
isFeasibleAssignment view moves = all (checkPosition view moves) neighbors
  where
    positions = Map.keys moves
    neighbors = usort $ concatMap (Player.neighbors view) positions
    usort = nub . sort

checkPosition :: PlayerView -> Map Pos Move -> Pos -> Bool
checkPosition view moves pos =
  case Player.item view pos of
    Nothing -> True
    Just Mine -> True
    Just (Empty numMines) -> checkNumMines view pos numMines moves

checkNumMines :: PlayerView -> Pos -> Int -> Map Pos Move -> Bool
checkNumMines view pos numMines moves =
  numMarkedMines <= numMines && (numUnopened + numMarkedMines) >= numMines
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
