{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Player
       (
         Move (OpenEmpty, MarkMine, GetPlay, PosInfo, RunRandom)
       , Player(Player, name, strategy)
       , Strategy
       , Name
       , makePlayer
       , runStrategy

       , module Free
       ) where

import Control.Monad.Trans.Free as Free (FreeT, FreeF(Pure, Free))
import Control.Monad.Trans.Free (runFreeT)

import Play (Play, Pos)
import Rand (Rand, Gen, runRand)

data Move next where
  OpenEmpty :: Pos -> ([Pos] -> next) -> Move next
  MarkMine  :: Pos -> next -> Move next
  GetPlay   :: (Play -> next) -> Move next
  PosInfo   :: [(Pos, String)] -> next -> Move next
  RunRandom :: Rand a -> (a -> next) -> Move next

deriving instance Functor Move

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

runStrategy :: Gen -> Strategy () -> IO (FreeF Move () (FreeT Move IO ()))
runStrategy gen s = loop s
  where loop s = runFreeT s >>= iter

        iter (Free (RunRandom r k)) = runRand r gen >>= loop . k
        iter move                   = return move
