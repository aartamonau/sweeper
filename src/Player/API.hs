module Player.API
       (
         Player
       , Strategy
       , makePlayer

       , openEmpty
       , markMine
       , getPlay
       , surrender
       , posInfo
       , io
       ) where


import Control.Monad.Trans.Free (liftF)
import Control.Monad.IO.Class (liftIO)

import Play (Play, Pos)
import Player (Player, Strategy, Move (..), makePlayer)

openEmpty :: Pos -> Strategy [Pos]
openEmpty p = liftF (OpenEmpty p id)

markMine :: Pos -> Strategy ()
markMine p = liftF (MarkMine p ())

getPlay :: Strategy Play
getPlay = liftF (GetPlay id)

surrender :: Strategy ()
surrender = return ()

posInfo :: [(Pos, String)] -> Strategy ()
posInfo ps = liftF (PosInfo ps ())

io :: IO a -> Strategy a
io = liftIO
