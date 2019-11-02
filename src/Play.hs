module Play
  ( Play(numMinesMarked, numUncovered)
  , PlayError(ErrorNoChange, ErrorFired, ErrorKilled)
  , Pos
  , Item(Mine, Empty)
  , newPlay
  , playRows
  , playColumns
  , openEmpty
  , markMine
  , isWon
  , isOpened
  , playItem
  , playBounds
  , playNeighbors
  , playNumMines
  , errorItem
  ) where


import Data.Array (Array, (!), (//), inRange, listArray)
import Data.List (foldl')
import Data.Maybe (isJust)

import Game (Game, Item(Empty, Mine), Pos)
import qualified Game as Game

data Play =
  Play
    { game :: {-# UNPACK #-} !Game

    , numMinesMarked :: !Int
    , numUncovered   :: !Int

    , field     :: !(Array Pos (Maybe Item))
    , errorMove :: !(Maybe Pos)
    }

data PlayError
  = ErrorFired
  | ErrorKilled
  | ErrorNoChange
  deriving (Show)

type PlayResult r = (Play, Either PlayError r)

newPlay :: Game -> Play
newPlay game =
  Play
    { game           = game
    , numMinesMarked = 0
    , numUncovered   = 0
    , field          = allClosed
    , errorMove      = Nothing
    }
  where
    allClosed = listArray bounds $ repeat Nothing
    bounds    = Game.bounds game

playRows :: Play -> Int
playRows = Game.rows . game

playColumns :: Play -> Int
playColumns = Game.columns . game

playBounds :: Play -> (Pos, Pos)
playBounds = Game.bounds . game

playItem :: Play -> Pos -> Maybe Item
playItem (Play {field}) p  = field ! p

playNeighbors :: Play -> Pos -> [Pos]
playNeighbors play p@(pi, pj) =
  [ q
  | di <- [-1, 0, 1]
  , dj <- [-1, 0, 1]
  , let q = (pi + di, pj + dj)
  , q /= p
  , inRange bounds q
  ]

  where
    bounds = playBounds play

playNumMines :: Play -> Int
playNumMines = Game.mines . game

openEmpty :: Play -> Pos -> PlayResult [Pos]
openEmpty play@(Play {game}) p
  | Just Mine <- item = retError play p ErrorFired
  | Just _    <- item = retError play p ErrorNoChange
  | otherwise         =
    case Game.getItem game p of
      Mine        -> retError play p ErrorKilled
      Empty mines ->
        let (ps, newPlay) = (openEmptyLoopEnter (p, mines) play)
        in ret newPlay ps
  where
    item = playItem play p

openEmptyLoopEnter :: (Pos, Int) -> Play -> ([Pos], Play)
openEmptyLoopEnter start@(p, _) play =
  openEmptyLoop start ([p], uncoverBox play p)

openEmptyLoop :: (Pos, Int) -> ([Pos], Play) -> ([Pos], Play)
openEmptyLoop (p, mines) acc@(seen, play)
  | mines == 0 = foldl' (flip openEmptyLoop) (acc', play') neighbors
  | otherwise  = acc
  where
    (i, j)    = p
    bounds    = playBounds play

    acc'      = map fst neighbors ++ seen
    play'     = foldl' uncoverBox play (map fst neighbors)

    neighbors = [ (np, count)
                | di <- [-1, 0, 1]
                , dj <- [-1, 0, 1]
                , let np = (i + di, j + dj)
                , np /= p
                , inRange bounds np
                , not (isOpened play np)
                , let Empty count = Game.getItem (game play) np
                ]

markMine :: Play -> Pos -> PlayResult ()
markMine play p
  | Just Mine <- item = retError play p ErrorNoChange
  | Just _    <- item = retError play p ErrorFired
  | otherwise         = ret (markBox play p) ()

  where
    item = playItem play p

isWon :: Play -> Bool
isWon play@(Play {game, numMinesMarked, numUncovered}) =
  numUncovered == numEmpty && numMinesMarked == playNumMines play
  where
    numEmpty = Game.rows game * Game.columns game - Game.mines game

isOpened :: Play -> Pos -> Bool
isOpened (Play {field}) p = isJust (field ! p)

uncoverBox :: Play -> Pos -> Play
uncoverBox play@(Play {game}) p =
  incNumUncovered $ setBox play p (Game.getItem game p)

markBox :: Play -> Pos -> Play
markBox play p = incMarkedMines $ setBox play p Mine

setBox :: Play -> Pos -> Item -> Play
setBox play@(Play {field}) p item = play {field = field // [(p, Just item)]}

incMarkedMines :: Play -> Play
incMarkedMines play@(Play {numMinesMarked}) =
  play {numMinesMarked = numMinesMarked + 1}

incNumUncovered :: Play -> Play
incNumUncovered play@(Play {numUncovered}) =
  play {numUncovered = numUncovered + 1}

retError :: Play -> Pos -> PlayError -> PlayResult a
retError play lastMove error = (newPlay, Left error)
  where
    newPlay = play {errorMove = Just lastMove}

ret :: Play -> a -> PlayResult a
ret play r = (play, Right r)

errorItem :: Play -> Maybe (Pos, Item)
errorItem (Play {game, errorMove}) = f <$> errorMove
  where
    f p = (p, Game.getItem game p)
