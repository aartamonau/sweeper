module Player.API
       (
         Player
       , Strategy
       , makePlayer

       , openEmpty
       , openMine
       , getPlay
       , surrender
       , draw
       , io
       ) where


import Control.Monad.Trans.Free (liftF)
import Control.Monad.IO.Class (liftIO)

import Draw (Draw)
import Game (Pos)
import Play (Play)
import Player (Player, Strategy, Move (..), makePlayer)

openEmpty :: Pos -> Strategy [Pos]
openEmpty p = liftF (OpenEmpty p id)

openMine :: Pos -> Strategy ()
openMine p = liftF (OpenMine p ())

getPlay :: Strategy Play
getPlay = liftF (GetPlay id)

surrender :: Strategy ()
surrender = return ()

draw :: [(Pos, Draw ())] -> Strategy ()
draw ds = liftF (Draw ds ())

io :: IO a -> Strategy a
io = liftIO
