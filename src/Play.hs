{-# LANGUAGE RecordWildCards #-}

module Play
       (
         Play (numMinesMarked, numUncovered)
       , PlayError (ErrorNoChange, ErrorFired, ErrorKilled)
       , newPlay
       , playRows
       , playColumns
       , openEmpty
       , markMine
       , isFinished
       , isOpened
       , playItem
       , playBounds
       , playNeighbors
       , playNumMines
       ) where


import Data.Array (Array, (!), (//), listArray, inRange)
import Data.List (foldl')
import Data.Maybe (isJust)

import Game (Game(mines, rows, columns),
             Item(Mine, Empty),
             Pos,
             gameItem, gameBounds)

data Play =
  Play { game :: Game

       , numMinesMarked :: Int
       , numUncovered   :: Int

       , field     :: Array Pos (Maybe Item)
       }

data PlayError = ErrorFired
               | ErrorKilled
               | ErrorNoChange
               deriving Show

type PlayResult r = Either PlayError r

newPlay :: Game -> Play
newPlay game =
  Play { game           = game
       , numMinesMarked = 0
       , numUncovered   = 0
       , field          = allClosed
       }
  where allClosed = listArray bounds $ repeat Nothing
        bounds    = gameBounds game

playRows :: Play -> Int
playRows = rows . game

playColumns :: Play -> Int
playColumns = columns . game

playBounds :: Play -> (Pos, Pos)
playBounds = gameBounds . game

playItem :: Play -> Pos -> Maybe Item
playItem (Play {..}) p  = field ! p

playNeighbors :: Play -> Pos -> [Pos]
playNeighbors play p@(pi, pj) =
  [q | di <- [-1, 0, 1],
       dj <- [-1, 0, 1],
       let q = (pi + di, pj + dj),
       q /= p, inRange bounds q]

  where bounds = playBounds play

playNumMines :: Play -> Int
playNumMines = mines . game

openEmpty :: Play -> Pos -> PlayResult ([Pos], Play)
openEmpty play@(Play {..}) p
  | Just Mine <- item = Left ErrorFired
  | Just _    <- item = Left ErrorNoChange
  | otherwise         =
      case gameItem game p of
       Mine          -> Left ErrorKilled
       (Empty mines) -> Right (openEmptyLoopEnter (p, mines) play)

  where item = playItem play p

openEmptyLoopEnter :: (Pos, Int) -> Play -> ([Pos], Play)
openEmptyLoopEnter start@(p, _) play = openEmptyLoop start ([p], uncoverBox play p)

openEmptyLoop :: (Pos, Int) -> ([Pos], Play) -> ([Pos], Play)
openEmptyLoop (p, mines) acc@(seen, play)
  | mines == 0 = foldl' (flip openEmptyLoop) (acc', play') neighbors
  | otherwise  = acc
  where (i, j)    = p
        bounds    = playBounds play

        acc'      = map fst neighbors ++ seen
        play'     = foldl' uncoverBox play (map fst neighbors)

        neighbors = [(np, count) | di <- [-1, 0, 1],
                                   dj <- [-1, 0, 1],
                                   let np = (i + di, j + dj),
                                   np /= p,
                                   inRange bounds np,
                                   not (isOpened play np),
                                   let Empty count = gameItem (game play) np]

markMine :: Play -> Pos -> PlayResult Play
markMine play p
  | Just Mine <- item = Left ErrorNoChange
  | Just _    <- item = Left ErrorFired
  | otherwise         = Right (markBox play p)

  where item = playItem play p

isFinished :: Play -> Bool
isFinished play@(Play {..}) =
  numUncovered == numEmpty && numMinesMarked == playNumMines play
  where numEmpty = rows game * columns game - mines game

isOpened :: Play -> Pos -> Bool
isOpened (Play {..}) p = isJust (field ! p)

uncoverBox :: Play -> Pos -> Play
uncoverBox play@(Play {..}) p =
  incNumUncovered $ setBox play p (gameItem game p)

markBox :: Play -> Pos -> Play
markBox play p = incMarkedMines $ setBox play p Mine

setBox :: Play -> Pos -> Item -> Play
setBox play@(Play {..}) p item = play {field = field // [(p, Just item)]}

incMarkedMines :: Play -> Play
incMarkedMines play@(Play {..}) = play {numMinesMarked = numMinesMarked + 1}

incNumUncovered :: Play -> Play
incNumUncovered play@(Play {..}) = play {numUncovered = numUncovered + 1}
