{-# LANGUAGE RecordWildCards #-}

module Play
       (
         Play (minesLeft, openedBoxes)
       , newPlay
       , playRows
       , playColumns
       , openEmpty
       , openMine
       , anyMinesLeft
       , isOpened
       , playItem
       , playBounds
       ) where


import Data.Array (Array, (!), (//), listArray)

import Game (Game(Game, mines, rows, columns),
             Item(Mine, Empty),
             Pos,
             gameItem, gameBounds)

data Play =
  Play { game :: Game

       , minesLeft   :: Int
       , openedBoxes :: Array Pos Bool
       }

data PlayError = ErrorAlreadyOpened
               | ErrorFired
               | ErrorKilled
               deriving Show

type PlayResult r = Either PlayError r

newPlay :: Game -> Play
newPlay game@(Game {..}) = Play game mines allClosed
  where allClosed = listArray bounds $ repeat False
        bounds    = gameBounds game

playRows :: Play -> Int
playRows = rows . game

playColumns :: Play -> Int
playColumns = columns . game

playBounds :: Play -> (Pos, Pos)
playBounds = gameBounds . game

playItem :: Play -> Pos -> Maybe Item
playItem play p
  | isOpened play p = Just (gameItem (game play) p)
  | otherwise       = Nothing

openEmpty :: Play -> Pos -> PlayResult ([Pos], Play)
openEmpty play@(Play {..}) p
  | isOpened play p            = Left ErrorAlreadyOpened
-- TODO: recursive open
  | Empty _ <- gameItem game p = Right ([p], openBox play p)
  | otherwise                  = Left ErrorKilled

openMine :: Play -> Pos -> PlayResult Play
openMine play@(Play {..}) p
  | isOpened play p         = Left ErrorAlreadyOpened
  | Mine <- gameItem game p = Right (decMines $ openBox play p)
  | otherwise               = Left ErrorFired

anyMinesLeft :: Play -> Bool
anyMinesLeft (Play {..}) = minesLeft /= 0

isOpened :: Play -> Pos -> Bool
isOpened (Play {..}) = (openedBoxes !)

openBox :: Play -> Pos -> Play
openBox play@(Play {..}) p = play {openedBoxes = openedBoxes // [(p, True)]}

decMines :: Play -> Play
decMines play@(Play {..}) = play {minesLeft = minesLeft - 1}
