{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Player
  ( Player(Player, name, strategy)
  , Name
  , MonadPlayer(openEmpty, markMine, getGame)
  , PlayerL
  , makePlayer
  ) where

import Game (Game, Pos)
import Rand (MonadRandom)

type Name = String
data Player =
  Player
    { name :: Name
    , strategy :: Pos -> PlayerL ()
    }

instance Show Player where
  show = name

class Monad m => MonadPlayer m where
  openEmpty :: Pos -> m [Pos]
  markMine :: Pos -> m ()
  getGame :: m Game

type PlayerL a = (forall m. (MonadPlayer m, MonadRandom m) => m a)

makePlayer :: Name -> (Pos -> PlayerL ()) -> Player
makePlayer = Player
