module Player.API
  ( Player
  , Strategy
  , makePlayer
  , openEmpty
  , markMine
  , getGame
  , surrender
  , posInfo
  , rand
  ) where

import Control.Monad.Trans.Free (liftF)

import Game (Game, Pos)
import Player
  ( Move(GetGame, MarkMine, OpenEmpty, PosInfo, RunRandom)
  , Player
  , Strategy
  , makePlayer
  )
import Rand (Rand)

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

rand :: Rand a -> Strategy a
rand r = liftF (RunRandom r id)
