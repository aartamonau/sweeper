module Player.CSP (
    player,
) where

import Data.Functor (void)
import Player (Player, PlayerL, Pos)
import qualified Player

player :: Player
player = Player.makePlayer "csp" strategy

strategy :: Pos -> PlayerL ()
strategy = void . Player.openEmpty
