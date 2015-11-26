{-# LANGUAGE DeriveFunctor #-}

module Player
       (
         Move (OpenEmpty, OpenMine, GetPlay, BoxDraw, Prompt)
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
               | BoxDraw Pos (Draw ()) next
               | Prompt next
               deriving Functor

type Player a = FreeT Move IO a
