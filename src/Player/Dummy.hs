module Player.Dummy (
    player,
) where

import Data.Functor (void)

import Player (Player, PlayerL, Pos)
import qualified Player
import Utils.Random (Coin (Heads, Tails))
import qualified Utils.Random as Random

player :: Player
player = Player.makePlayer "dummy" strategy

strategy :: Pos -> PlayerL ()
strategy start = do
    view <- Player.getPlayerView
    let dims = (Player.numRows view, Player.numColumns view)

    _ <- Player.openEmpty start
    loop dims

loop :: (Int, Int) -> PlayerL ()
loop dims = randomMove dims >> loop dims

randomMove :: (Int, Int) -> PlayerL ()
randomMove (rows, columns) = do
    i <- Random.getRandomR (0, rows -1)
    j <- Random.getRandomR (0, columns -1)

    let pos = (i, j)
    Random.coin >>= \case
        Heads -> void $ Player.openEmpty pos
        Tails -> Player.markMine pos
