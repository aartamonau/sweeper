module Player.API
  ( Player
  , Strategy
  , makePlayer
  , openEmpty
  , markMine
  , getGame
  , surrender
  , posInfo
  , st
  , rand
  , module Rand
  ) where

import Control.Monad.ST (RealWorld, ST)
import Control.Monad.Trans.Free (liftF)

import Game (Game, Pos)
import Player
  ( Move(GetGame, MarkMine, OpenEmpty, PosInfo, RunRandom, RunST)
  , Player
  , Strategy
  , makePlayer
  )
import Rand (Rand, uniform, uniformR)

openEmpty :: Pos -> Strategy [Pos]
openEmpty p = liftF (OpenEmpty p id)

markMine :: Pos -> Strategy ()
markMine p = liftF (MarkMine p ())

getGame :: Strategy Game
getGame = liftF (GetGame id)

surrender :: Strategy ()
surrender = return ()

posInfo :: [(Pos, String)] -> Strategy ()
posInfo ps = liftF (PosInfo ps ())

st :: ST RealWorld a -> Strategy a
st action = liftF (RunST action id)

rand :: Rand a -> Strategy a
rand r = liftF (RunRandom r id)
