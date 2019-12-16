{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}

module Player
  ( Move(OpenEmpty, MarkMine, GetGame, PosInfo, RunRandom)
  , Player(Player, name, strategy)
  , Strategy
  , Name
  , MonadPlayer(openEmpty, markMine, getGame, surrender, posInfo, rand)
  , FreeF(Free, Pure)
  , makePlayer
  , runStrategy
  ) where

import Control.Monad.Trans.Free
  ( FreeF(Free, Pure)
  , FreeT
  , MonadFree
  , liftF
  , runFreeT
  )
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

class Monad m => MonadPlayer m where
  openEmpty :: Pos -> m [Pos]
  markMine :: Pos -> m ()
  getGame :: m Game
  surrender :: m ()
  posInfo :: [(Pos, String)] -> m ()
  rand :: Rand a -> m a

instance MonadPlayer Strategy where
  openEmpty p = liftMove (OpenEmpty p id)
  markMine p = liftMove (MarkMine p ())
  getGame = liftMove (GetGame id)
  surrender = return ()
  posInfo ps = liftMove (PosInfo ps ())
  rand r = liftMove (RunRandom r id)

liftMove :: Move a -> Strategy a
liftMove = Strategy . liftF

makePlayer :: Name -> (forall m. MonadPlayer m => Pos -> m ()) -> Player
makePlayer = Player

runStrategy :: Gen -> Strategy () -> IO (FreeF Move () (Strategy ()))
runStrategy gen s = fmap (second Strategy) (loop (unStrategy s))
  where
    loop s = runFreeT s >>= iter

    iter (Free (RunRandom r k)) = runRand r gen >>= loop . k
    iter move                   = return move
