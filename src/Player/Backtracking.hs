module Player.Backtracking
  ( player
  ) where

import Player (Player, PlayerL, Pos)
import qualified Player as Player

player :: Player
player = Player.makePlayer "backtracking" strategy

strategy :: Pos -> PlayerL ()
strategy start = Player.openEmpty start >> return ()
