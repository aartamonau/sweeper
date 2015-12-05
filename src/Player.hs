{-# LANGUAGE DeriveFunctor #-}

module Player
       (
         Move (OpenEmpty, MarkMine, GetPlay, PosInfo)
       , Player(Player, name, strategy)
       , Strategy
       , Name
       , makePlayer

       , module Control.Monad.Trans.Free
       ) where

import Control.Monad.Trans.Free (FreeT, FreeF(Pure, Free), runFreeT)

import Game (Pos)
import Play (Play)

data Move next = OpenEmpty Pos ([Pos] -> next)
               | MarkMine Pos next
               | GetPlay (Play -> next)
               | PosInfo [(Pos, String)] next
               deriving Functor

type Name       = String
type Strategy a = FreeT Move IO a

data Player =
  Player { name     :: Name
         , strategy :: Pos -> Strategy ()
         }

makePlayer :: Name -> (Pos -> Strategy ()) -> Player
makePlayer = Player
