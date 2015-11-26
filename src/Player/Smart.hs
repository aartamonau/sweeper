module Player.Smart
       (
         newPlayer
       ) where

import Draw (dimRect)
import Game (Pos)
import Player.API (Player, openEmpty, boxDraw, prompt)

newPlayer :: Pos -> Player ()
newPlayer start = dim start >> dim (0, 0) >> prompt >> openEmpty start >> return ()
  where dim p = boxDraw p (dimRect 0.5 (0,0,1,1))
