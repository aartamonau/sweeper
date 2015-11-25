{-# LANGUAGE RecordWildCards #-}

module Play
       (
         Play (minesLeft, openedBoxes)
       , PlayError (ErrorAlreadyOpened, ErrorFired, ErrorKilled)
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


import Data.Array (Array, (!), (//), listArray, inRange)
import Data.List (foldl')

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
  | isOpened play p                = Left ErrorAlreadyOpened
  | Empty mines <- gameItem game p = Right (openEmptyLoop (p, mines) ([], play))
  | otherwise                      = Left ErrorKilled

openEmptyLoop :: (Pos, Int) -> ([Pos], Play) -> ([Pos], Play)
openEmptyLoop (p, mines) (acc, play)
  | mines == 0 = foldl' (flip openEmptyLoop) (acc', play') neighbors
  | otherwise  = (acc', play')
  where acc'  = p : acc
        play' = openBox play p

        (i, j)    = p
        bounds    = playBounds play
        neighbors = [(np, count) | di <- [-1, 0, 1],
                                   dj <- [-1, 0, 1],
                                   let np = (i + di, j + dj),
                                   np /= p,
                                   inRange bounds np,
                                   not (isOpened play np),
                                   let Empty count = gameItem (game play) np]

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
