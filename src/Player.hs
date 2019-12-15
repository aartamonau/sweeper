{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Player
  ( Move(OpenEmpty, MarkMine, GetGame, PosInfo, RunRandom)
  , Player(Player, name, strategy)
  , Strategy
  , Name
  , makePlayer
  , runStrategy
  , module Free
  ) where

import Control.Monad.Trans.Free as Free (FreeF(Free, Pure), FreeT, MonadFree)
import Control.Monad.Trans.Free (runFreeT)
import Data.Bifunctor (second)

import Game (Game, Pos)
import Rand (Gen, Rand, runRand)

data Move next where
  OpenEmpty :: Pos -> ([Pos] -> next) -> Move next
  MarkMine  :: Pos -> next -> Move next
  GetGame   :: (Game -> next) -> Move next
  PosInfo   :: [(Pos, String)] -> next -> Move next
  RunRandom :: Rand a -> (a -> next) -> Move next

deriving instance Functor Move

type Name = String
newtype Strategy a =
  Strategy
    { unStrategy :: FreeT Move IO a
    }
  deriving (Functor, Applicative, Monad, MonadFree Move)

data Player =
  Player
    { name :: Name
    , strategy :: Pos -> Strategy ()
    }

instance Show Player where
  show = name

makePlayer :: Name -> (Pos -> Strategy ()) -> Player
makePlayer = Player

runStrategy :: Gen -> Strategy () -> IO (FreeF Move () (Strategy ()))
runStrategy gen s = fmap (second Strategy) (loop (unStrategy s))
  where
    loop s = runFreeT s >>= iter

    iter (Free (RunRandom r k)) = runRand r gen >>= loop . k
    iter move                   = return move
