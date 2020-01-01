{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Player
  ( Move(OpenEmpty, MarkMine, GetGame, PosInfo, RunRandom)
  , Player(Player, name, strategy)
  , Strategy
  , Name
  , MonadPlayer(openEmpty, markMine, getGame, posInfo)
  , PlayerL
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
import Rand (MonadRandom(getRandom, getRandomR, getRandomRs, getRandoms))

type Rand a = forall m. MonadRandom m => m a

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
  posInfo :: [(Pos, String)] -> m ()

instance MonadPlayer Strategy where
  openEmpty p = liftMove (OpenEmpty p id)
  markMine p = liftMove (MarkMine p ())
  getGame = liftMove (GetGame id)
  posInfo ps = liftMove (PosInfo ps ())

instance MonadRandom Strategy where
  getRandomRs bounds = liftMove (RunRandom (getRandomRs bounds) id)
  getRandom = liftMove (RunRandom getRandom id)
  getRandomR bounds = liftMove (RunRandom (getRandomR bounds) id)
  getRandoms = liftMove (RunRandom getRandoms id)

type PlayerL a = (forall m. (MonadPlayer m, MonadRandom m) => m a)

liftMove :: Move a -> Strategy a
liftMove = Strategy . liftF

makePlayer :: Name -> (Pos -> PlayerL ()) -> Player
makePlayer = Player

runStrategy :: Strategy () -> IO (FreeF Move () (Strategy ()))
runStrategy s = fmap (second Strategy) (loop (unStrategy s))
  where
    loop s = runFreeT s >>= iter

    iter (Free (RunRandom r k)) = r >>= loop . k
    iter move                   = return move
