module Player
  ( Item(Mine, Empty)
  , Name
  , MonadPlayer(openEmpty, markMine, getPlayerView)
  , Player(Player, name, strategy)
  , PlayerL
  , PlayerView
  , Pos
  , bounds
  , isOpened
  , item
  , makePlayer
  , makePlayerView
  , neighbors
  , numMines
  , numMinesMarked
  , numRows
  , numColumns
  , numUnopened
  ) where

import Game (Game, Item(Mine, Empty), Pos)
import qualified Game
import Utils.Random (MonadRandom)

type Name = String
data Player =
  Player
    { name :: Name
    , strategy :: Pos -> PlayerL ()
    }

newtype PlayerView = PlayerView { unPlayerView :: Game }

instance Show Player where
  show = name

class Monad m => MonadPlayer m where
  openEmpty :: Pos -> m [Pos]
  markMine :: Pos -> m ()
  getPlayerView :: m PlayerView

type PlayerL a = (forall m. (MonadPlayer m, MonadRandom m) => m a)

makePlayer :: Name -> (Pos -> PlayerL ()) -> Player
makePlayer = Player

makePlayerView :: Game -> PlayerView
makePlayerView = PlayerView

bounds :: PlayerView -> (Pos, Pos)
bounds = Game.bounds . unPlayerView

numRows :: PlayerView -> Int
numRows = Game.numRows . unPlayerView

numColumns :: PlayerView -> Int
numColumns = Game.numColumns . unPlayerView

isOpened :: PlayerView -> Pos -> Bool
isOpened view = Game.isOpened (unPlayerView view)

item :: PlayerView -> Pos -> Maybe Item
item view = Game.item (unPlayerView view)

neighbors :: PlayerView -> Pos -> [Pos]
neighbors view = Game.neighbors (unPlayerView view)

numMines :: PlayerView -> Int
numMines = Game.numMines . unPlayerView

numMinesMarked :: PlayerView -> Int
numMinesMarked = Game.numMinesMarked . unPlayerView

numUnopened :: PlayerView -> Int
numUnopened = Game.numUnopened . unPlayerView
