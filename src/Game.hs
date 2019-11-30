module Game
  ( Game(numMinesMarked)
  , PlayError(ErrorKilled, ErrorAlreadyPlayed)
  , Pos
  , Item(Mine, Empty)
  , random
  , numRows
  , numColumns
  , numMines
  , openEmpty
  , markMine
  , isWon
  , isOpened
  , item
  , bounds
  , neighbors
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

data Game =
  Game
    { numRows    :: Int
    , numColumns :: Int
    , numMines   :: Int

    , numMinesMarked :: Int
    , numUncovered   :: Int

    , field      :: MineField
    , fieldState :: Array Pos (Maybe Item)
    , errorMove  :: Maybe Pos
    }

data PlayError
  = ErrorKilled
  | ErrorAlreadyPlayed
  deriving (Show)

type PlayResult r = (Game, Either PlayError r)

mkMineField :: (Pos, Pos) -> [Pos] -> MineField
mkMineField bounds mines = listArray bounds $ [item p | p <- range bounds]
  where
    mineMap = accumArray (||) False bounds [(p, True) | p <- mines]

    hasMine = (mineMap !)
    item p | hasMine p = Mine
           | otherwise = Empty (length $ filter hasMine (neighbors' bounds p))

random :: Int -> Int -> Int -> Pos -> Int -> Rand Game
random rows columns numMines start buffer = do
  let bounds    = ((0, 0), (rows-1, columns-1))
  let positions = [p | p <- range bounds, not (isClose start p)]
  mines <- randomSubset numMines positions

  return $ Game
             { numRows        = rows
             , numColumns     = columns
             , numMines       = length mines
             , numMinesMarked = 0
             , numUncovered   = 0
             , field          = mkMineField bounds mines
             , fieldState     = listArray bounds $ repeat Nothing
             , errorMove      = Nothing
             }
  where
    isClose (si, sj) (i, j) = abs (si - i) <= buffer && abs (sj - j) <= buffer

bounds :: Game -> (Pos, Pos)
bounds (Game {numRows, numColumns}) = ((0, 0), (numRows - 1, numColumns - 1))

item :: Game -> Pos -> Maybe Item
item (Game {fieldState}) p  = fieldState ! p

neighbors :: Game -> Pos -> [Pos]
neighbors game = neighbors' (bounds game)

neighbors' :: (Pos, Pos) -> Pos -> [Pos]
neighbors' bounds p@(pi, pj) =
  [ q
  | di <- [-1, 0, 1]
  , dj <- [-1, 0, 1]
  , let q = (pi + di, pj + dj)
  , q /= p
  , inRange bounds q
  ]

openEmpty :: Game -> Pos -> PlayResult [Pos]
openEmpty game@(Game {field}) p
  | Just _ <- itm = retError game p ErrorAlreadyPlayed
  | otherwise     =
    case field ! p of
      Mine        -> retError game p ErrorKilled
      Empty mines ->
        let (ps, newGame) = (openEmptyLoopEnter (p, mines) game)
        in ret newGame ps
  where
    itm = item game p

openEmptyLoopEnter :: (Pos, Int) -> Game -> ([Pos], Game)
openEmptyLoopEnter start@(p, _) game =
  openEmptyLoop start ([p], uncoverBox game p)

openEmptyLoop :: (Pos, Int) -> ([Pos], Game) -> ([Pos], Game)
openEmptyLoop (p, mines) acc@(seen, game)
  | mines == 0 = foldl' (flip openEmptyLoop) (acc', game') toExamine
  | otherwise  = acc
  where
    acc'      = map fst toExamine ++ seen
    game'     = foldl' uncoverBox game (map fst toExamine)

    toExamine = [ (np, count)
                | np <- neighbors game p
                , not (isOpened game np)
                , let Empty count = field game ! np
                ]

markMine :: Game -> Pos -> PlayResult ()
markMine game p
  | Just _ <- itm = retError game p ErrorAlreadyPlayed
  | otherwise     = ret (markBox game p) ()

  where
    itm = item game p

isWon :: Game -> Bool
isWon (Game {numRows, numColumns, numMines, numMinesMarked, numUncovered}) =
  numUncovered == numEmpty && numMinesMarked == numMines
  where
    numEmpty = numRows * numColumns - numMines

isOpened :: Game -> Pos -> Bool
isOpened game p = isJust (item game p)

uncoverBox :: Game -> Pos -> Game
uncoverBox game@(Game {field}) p =
  incNumUncovered $ setBox game p (field ! p)

markBox :: Game -> Pos -> Game
markBox game p = incMarkedMines $ setBox game p Mine

setBox :: Game -> Pos -> Item -> Game
setBox game@(Game {fieldState}) p item =
  game {fieldState = fieldState // [(p, Just item)]}

incMarkedMines :: Game -> Game
incMarkedMines game@(Game {numMinesMarked}) =
  game {numMinesMarked = numMinesMarked + 1}

incNumUncovered :: Game -> Game
incNumUncovered game@(Game {numUncovered}) =
  game {numUncovered = numUncovered + 1}

retError :: Game -> Pos -> PlayError -> PlayResult a
retError game lastMove error = (newGame, Left error)
  where
    newGame = game {errorMove = Just lastMove}

ret :: Game -> a -> PlayResult a
ret game r = (game, Right r)

errorItem :: Game -> Maybe (Pos, Item)
errorItem (Game {field, errorMove}) = f <$> errorMove
  where
    f p = (p, field ! p)
