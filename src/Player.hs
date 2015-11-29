{-# LANGUAGE DeriveFunctor #-}

module Player
       (
         Move (OpenEmpty, OpenMine, GetPlay, Draw)
       , Player

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

type Player a = FreeT Move IO a
