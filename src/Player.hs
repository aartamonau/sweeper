{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}

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

import Play (Play, Pos)

data Move next where
  OpenEmpty :: Pos -> ([Pos] -> next) -> Move next
  MarkMine  :: Pos -> next -> Move next
  GetPlay   :: (Play -> next) -> Move next
  PosInfo   :: [(Pos, String)] -> next -> Move next
  deriving Functor

type Name       = String
type Strategy a = FreeT Move IO a

data Player =
  Player { name     :: Name
         , strategy :: Pos -> Strategy ()
         }

instance Show Player where
  show = name

makePlayer :: Name -> (Pos -> Strategy ()) -> Player
makePlayer = Player
