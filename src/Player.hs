{-# LANGUAGE DeriveFunctor #-}

module Player
       (
         Move (OpenEmpty, OpenMine, GetPlay, Draw)
       , Player(Player, name, strategy)
       , Strategy
       , Name
       , makePlayer

       , module Control.Monad.Trans.Free
       ) where

import Control.Monad.Trans.Free (FreeT, FreeF(Pure, Free), runFreeT)

import Draw (Draw)
import Game (Pos)
import Play (Play)

data Move next = OpenEmpty Pos ([Pos] -> next)
               | OpenMine Pos next
               | GetPlay (Play -> next)
               | Draw [(Pos, Draw ())] next
               deriving Functor

type Name       = String
type Strategy a = FreeT Move IO a

data Player =
  Player { name     :: Name
         , strategy :: Strategy ()
         }

makePlayer :: Name -> Strategy () -> Player
makePlayer = Player
