module Player.Smart
       (
         newPlayer
       ) where

import Game (Pos)
import Player.API (Player, openEmpty)

newPlayer :: Pos -> Player ()
newPlayer start = openEmpty start >> return ()
