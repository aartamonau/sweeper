module Player.Backtracking
  ( player
  ) where

import Data.Set (Set, (\\))
import qualified Data.Set as Set
import Player (Player, PlayerL, PlayerView, Pos)
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
  = Mine
  | Empty

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
    Mine -> Player.markMine p >> return []
    Empty -> Player.openEmpty p

findMove :: State -> Maybe (Pos, Move)
findMove (State {frontier}) = Just (Set.findMin frontier, Empty)

unopenedNeighbors :: PlayerView -> [Pos] -> Set Pos
unopenedNeighbors view = Set.fromList . filter isUnopened . concatMap neighbors
  where
    isUnopened = not . Player.isOpened view
    neighbors = Player.neighbors view
