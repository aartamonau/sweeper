module Play
  ( Play(numMinesMarked, numUncovered)
  , PlayError(ErrorNoChange, ErrorFired, ErrorKilled)
  , Pos
  , Item(Mine, Empty)
  , newPlay
  , rows
  , columns
  , openEmpty
  , markMine
  , isWon
  , isOpened
  , item
  , bounds
  , neighbors
  , numMines
  , errorItem
  ) where


import Data.Array (Array, (!), (//), inRange, listArray)
import Data.List (foldl')
import Data.Maybe (isJust)

import Game (Game, Item(Empty, Mine), Pos)
import qualified Game as Game

data Play =
  Play
    { game :: Game

    , numMinesMarked :: Int
    , numUncovered   :: Int

    , fieldState :: Array Pos (Maybe Item)
    , errorMove  :: Maybe Pos
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
    , fieldState     = allClosed
    , errorMove      = Nothing
    }
  where
    allClosed = listArray bounds $ repeat Nothing
    bounds    = Game.bounds game

rows :: Play -> Int
rows = Game.rows . game

columns :: Play -> Int
columns = Game.columns . game

bounds :: Play -> (Pos, Pos)
bounds = Game.bounds . game

item :: Play -> Pos -> Maybe Item
item (Play {fieldState}) p  = fieldState ! p

neighbors :: Play -> Pos -> [Pos]
neighbors play p@(pi, pj) =
  [ q
  | di <- [-1, 0, 1]
  , dj <- [-1, 0, 1]
  , let q = (pi + di, pj + dj)
  , q /= p
  , inRange (bounds play) q
  ]

numMines :: Play -> Int
numMines = Game.mines . game

openEmpty :: Play -> Pos -> PlayResult [Pos]
openEmpty play@(Play {game}) p
  | Just Mine <- itm = retError play p ErrorFired
  | Just _    <- itm = retError play p ErrorNoChange
  | otherwise        =
    case Game.getItem game p of
      Mine        -> retError play p ErrorKilled
      Empty mines ->
        let (ps, newPlay) = (openEmptyLoopEnter (p, mines) play)
        in ret newPlay ps
  where
    itm = item play p

openEmptyLoopEnter :: (Pos, Int) -> Play -> ([Pos], Play)
openEmptyLoopEnter start@(p, _) play =
  openEmptyLoop start ([p], uncoverBox play p)

openEmptyLoop :: (Pos, Int) -> ([Pos], Play) -> ([Pos], Play)
openEmptyLoop (p, mines) acc@(seen, play)
  | mines == 0 = foldl' (flip openEmptyLoop) (acc', play') neighbors
  | otherwise  = acc
  where
    (i, j)    = p
    acc'      = map fst neighbors ++ seen
    play'     = foldl' uncoverBox play (map fst neighbors)
    neighbors = [ (np, count)
                | di <- [-1, 0, 1]
                , dj <- [-1, 0, 1]
                , let np = (i + di, j + dj)
                , np /= p
                , inRange (bounds play) np
                , not (isOpened play np)
                , let Empty count = Game.getItem (game play) np
                ]

markMine :: Play -> Pos -> PlayResult ()
markMine play p
  | Just Mine <- itm = retError play p ErrorNoChange
  | Just _    <- itm = retError play p ErrorFired
  | otherwise        = ret (markBox play p) ()

  where
    itm = item play p

isWon :: Play -> Bool
isWon play@(Play {game, numMinesMarked, numUncovered}) =
  numUncovered == numEmpty && numMinesMarked == numMines play
  where
    numEmpty = Game.rows game * Game.columns game - Game.mines game

isOpened :: Play -> Pos -> Bool
isOpened (Play {fieldState}) p = isJust (fieldState ! p)

uncoverBox :: Play -> Pos -> Play
uncoverBox play@(Play {game}) p =
  incNumUncovered $ setBox play p (Game.getItem game p)

markBox :: Play -> Pos -> Play
markBox play p = incMarkedMines $ setBox play p Mine

setBox :: Play -> Pos -> Item -> Play
setBox play@(Play {fieldState}) p item =
  play {fieldState = fieldState // [(p, Just item)]}

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
