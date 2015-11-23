{-# LANGUAGE DeriveFunctor #-}

module Player
       (
         Move (OpenEmpty, OpenMine, GetPlay)
       , Player

       , module Control.Monad.Trans.Free
       ) where

import Control.Monad.Trans.Free (FreeT, FreeF(Pure, Free), runFreeT)

import Game (Pos)
import Play (Play)

data Move next = OpenEmpty Pos ([Pos] -> next)
               | OpenMine Pos next
               | GetPlay (Play -> next)
               deriving Functor

type Player a = FreeT Move IO a
