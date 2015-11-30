{-# LANGUAGE RecordWildCards #-}

module Play
       (
         Play (minesLeft, openedBoxes)
       , PlayError (ErrorFired, ErrorKilled)
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

data PlayError = ErrorFired
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
  | isOpened play p     =
      case item of
       Empty _ -> Right ([], play)
       _       -> Left ErrorKilled
  | Empty mines <- item = Right (openEmptyLoopEnter (p, mines) play)
  | otherwise           = Left ErrorKilled

  where item = gameItem game p

openEmptyLoopEnter :: (Pos, Int) -> Play -> ([Pos], Play)
openEmptyLoopEnter start@(p, _) play = openEmptyLoop start ([p], openBox play p)

openEmptyLoop :: (Pos, Int) -> ([Pos], Play) -> ([Pos], Play)
openEmptyLoop (p, mines) acc@(seen, play)
  | mines == 0 = foldl' (flip openEmptyLoop) (acc', play') neighbors
  | otherwise  = acc
  where (i, j)    = p
        bounds    = playBounds play

        acc'      = map fst neighbors ++ seen
        play'     = foldl' openBox play (map fst neighbors)

        neighbors = [(np, count) | di <- [-1, 0, 1],
                                   dj <- [-1, 0, 1],
                                   let np = (i + di, j + dj),
                                   np /= p,
                                   inRange bounds np,
                                   not (isOpened play np),
                                   let Empty count = gameItem (game play) np]

openMine :: Play -> Pos -> PlayResult Play
openMine play@(Play {..}) p
  | isOpened play p =
      case item of
       Mine -> Right play
       _    -> Left ErrorFired
  | Mine <- item    = Right (decMines $ openBox play p)
  | otherwise       = Left ErrorFired

  where item = gameItem game p

anyMinesLeft :: Play -> Bool
anyMinesLeft (Play {..}) = minesLeft /= 0

isOpened :: Play -> Pos -> Bool
isOpened (Play {..}) = (openedBoxes !)

openBox :: Play -> Pos -> Play
openBox play@(Play {..}) p = play {openedBoxes = openedBoxes // [(p, True)]}

decMines :: Play -> Play
decMines play@(Play {..}) = play {minesLeft = minesLeft - 1}
