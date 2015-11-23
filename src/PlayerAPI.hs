module PlayerAPI
       (
         Player
       , openEmpty
       , openMine
       , getPlay
       , surrender
       , io
       ) where


import Control.Monad.Trans.Free (liftF)
import Control.Monad.IO.Class (liftIO)

import Game (Pos)
import Play (Play)
import Player (Player, Move (..))

openEmpty :: Pos -> Player [Pos]
openEmpty p = liftF (OpenEmpty p id)

openMine :: Pos -> Player ()
openMine p = liftF (OpenMine p ())

getPlay :: Player Play
getPlay = liftF (GetPlay id)

surrender :: Player ()
surrender = return ()

io :: IO a -> Player a
io = liftIO
