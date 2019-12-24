module Game
  ( Game
  , PlayError(ErrorKilled, ErrorAlreadyPlayed)
  , Pos
  , Item(Mine, Empty)
  , random
  , numRows
  , numColumns
  , numMines
  , numMinesMarked
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
import Data.Set (Set)
import qualified Data.Set as Set

import Rand (MonadRandom, randomSubset)

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
    , numOpened      :: Int

    , field     :: MineField
    , opened    :: Array Pos Bool
    , errorMove :: Maybe Pos
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

random :: MonadRandom m => Int -> Int -> Int -> Pos -> Int -> m Game
random rows columns numMines start buffer = do
  let bounds    = ((0, 0), (rows-1, columns-1))
  let positions = [p | p <- range bounds, not (isClose start p)]
  mines <- randomSubset numMines positions

  return $ Game
             { numRows        = rows
             , numColumns     = columns
             , numMines       = length mines
             , numMinesMarked = 0
             , numOpened      = 0
             , field          = mkMineField bounds mines
             , opened         = listArray bounds $ repeat False
             , errorMove      = Nothing
             }
  where
    isClose (si, sj) (i, j) = abs (si - i) <= buffer && abs (sj - j) <= buffer

bounds :: Game -> (Pos, Pos)
bounds (Game {numRows, numColumns}) = ((0, 0), (numRows - 1, numColumns - 1))

item :: Game -> Pos -> Maybe Item
item game@(Game {field}) p
  | isOpened game p = Just $ field ! p
  | otherwise = Nothing

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
  | Just _ <- item game p = retError game p ErrorAlreadyPlayed
  | otherwise =
    case field ! p of
      Mine -> retError game p ErrorKilled
      Empty _ ->
        let ps = openEmptyLoop game p
         in ret (foldl' doOpen game ps) ps

openEmptyLoop :: Game -> Pos -> [Pos]
openEmptyLoop game@(Game {field}) p = Set.toList (go Set.empty p)
  where
    go :: Set Pos -> Pos -> Set Pos
    go acc p
      | Empty 0 <- item =
        let ns =
              [ np
              | np <- neighbors game p
              , not (isOpened game np)
              , not (Set.member np acc)
              ]
         in foldl' go acc' ns
      | Empty _ <- item = acc'
      | otherwise = error "impossible"
      where
        item = field ! p
        acc' = Set.insert p acc

markMine :: Game -> Pos -> PlayResult ()
markMine game p
  | Just _ <- itm = retError game p ErrorAlreadyPlayed
  | otherwise     = ret (doOpen game p) ()

  where
    itm = item game p

isWon :: Game -> Bool
isWon (Game {numRows, numColumns, numMines, numMinesMarked, numOpened}) =
  numOpened == numRows * numColumns && numMinesMarked == numMines

isOpened :: Game -> Pos -> Bool
isOpened (Game {opened}) p = opened ! p

doOpen :: Game -> Pos -> Game
doOpen game@(Game {field, opened, numOpened, numMinesMarked}) p
  | isOpened game p = game
  | otherwise =
    game
      { opened = opened // [(p, True)]
      , numOpened = numOpened + 1
      , numMinesMarked = numMinesMarked'
      }

  where
    numMinesMarked' | Mine <- field ! p = numMinesMarked + 1
                    | otherwise = numMinesMarked

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
