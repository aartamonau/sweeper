module Play
  ( Play(numMinesMarked, numUncovered)
  , PlayError(ErrorNoChange, ErrorFired, ErrorKilled)
  , Pos
  , Item(Mine, Empty)
  , random
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


import Data.Array (Array, (!), (//), accumArray, inRange, listArray, range)
import Data.List (foldl')
import Data.Maybe (isJust)

import Rand (Rand, randomSubset)

type Pos = (Int, Int)
type MineField = Array Pos Item

data Item
  = Mine
  | Empty Int
  deriving (Show, Eq)

data Play =
  Play
    { rows    :: Int
    , columns :: Int
    , mines   :: Int

    , numMinesMarked :: Int
    , numUncovered   :: Int

    , field      :: MineField
    , fieldState :: Array Pos (Maybe Item)
    , errorMove  :: Maybe Pos
    }

data PlayError
  = ErrorFired
  | ErrorKilled
  | ErrorNoChange
  deriving (Show)

type PlayResult r = (Play, Either PlayError r)

mkMineField :: (Pos, Pos) -> [Pos] -> MineField
mkMineField bounds mines = listArray bounds $ [item p | p <- range bounds]
  where
    mineMap = accumArray (||) False bounds [(p, True) | p <- mines]

    neighbors (i, j) = [p | di <- [-1, 0, 1],
                        dj <- [-1, 0, 1],
                        let p = (i + di, j + dj),
                        p /= (i, j),
                        inRange bounds p]

    hasMine = (mineMap !)
    item p | hasMine p = Mine
           | otherwise = Empty (length $ filter hasMine (neighbors p))

random :: Int -> Int -> Int -> Pos -> Int -> Rand Play
random rows columns numMines start buffer = do
  let bounds    = ((0, 0), (rows-1, columns-1))
  let positions = [p | p <- range bounds, not (isClose start p)]
  mines <- randomSubset numMines positions

  return $ Play
             { rows           = rows
             , columns        = columns
             , mines          = length mines
             , numMinesMarked = 0
             , numUncovered   = 0
             , field          = mkMineField bounds mines
             , fieldState     = listArray bounds $ repeat Nothing
             , errorMove      = Nothing
             }
  where
    isClose (si, sj) (i, j) = abs (si - i) <= buffer && abs (sj - j) <= buffer

bounds :: Play -> (Pos, Pos)
bounds (Play {rows, columns}) = ((0, 0), (rows - 1, columns - 1))

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
numMines (Play {mines}) = mines

openEmpty :: Play -> Pos -> PlayResult [Pos]
openEmpty play@(Play {field}) p
  | Just Mine <- itm = retError play p ErrorFired
  | Just _    <- itm = retError play p ErrorNoChange
  | otherwise        =
    case field ! p of
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
                , let Empty count = field play ! np
                ]

markMine :: Play -> Pos -> PlayResult ()
markMine play p
  | Just Mine <- itm = retError play p ErrorNoChange
  | Just _    <- itm = retError play p ErrorFired
  | otherwise        = ret (markBox play p) ()

  where
    itm = item play p

isWon :: Play -> Bool
isWon (Play {rows, columns, mines, numMinesMarked, numUncovered}) =
  numUncovered == numEmpty && numMinesMarked == mines
  where
    numEmpty = rows * columns - mines

isOpened :: Play -> Pos -> Bool
isOpened (Play {fieldState}) p = isJust (fieldState ! p)

uncoverBox :: Play -> Pos -> Play
uncoverBox play@(Play {field}) p =
  incNumUncovered $ setBox play p (field ! p)

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
errorItem (Play {field, errorMove}) = f <$> errorMove
  where
    f p = (p, field ! p)
