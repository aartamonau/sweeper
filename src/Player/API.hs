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
       , rand

       , module Rand
       ) where


import Control.Monad.Trans.Free (liftF)

import Play (Play, Pos)
import Player (Player, Strategy, Move (..), makePlayer)
import Rand (Rand, uniform, uniformR)

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

rand :: Rand a -> Strategy a
rand r = liftF (RunRandom r id)
