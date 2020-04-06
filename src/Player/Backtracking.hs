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
findMove (State {frontier, view}) =
  case filter isInfeasible moves of
    [] -> Nothing
    ((pos, move):_) -> Just (pos, flipMove move)
  where
    ps = Set.toList frontier
    moves = [(p, MoveMine) | p <- ps] ++ [(p, MoveEmpty) | p <- ps]
    isInfeasible = not . uncurry (isFeasibleMove view)

isFeasibleMove :: PlayerView -> Pos -> Move -> Bool
isFeasibleMove view pos move =
  isFeasibleAssignment view (Map.singleton pos move)

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
