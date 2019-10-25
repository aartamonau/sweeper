module Game
       (
         Game (Game, rows, columns, field, mines)
       , MineField
       , Pos
       , Item (Mine, Empty)
       , random
       , bounds
       , getItem
       ) where

import Data.Array (Array, (!), inRange, range, accumArray, listArray)

import Rand (Rand, randomSubset)

data Game =
  Game { rows    :: !Int
       , columns :: !Int
       , mines   :: !Int
       , field   :: !MineField
       }
  deriving Show

type Pos = (Int, Int)
type MineField = Array Pos Item

data Item = Mine
          | Empty Int
          deriving (Show, Eq)

getItem :: Game -> Pos -> Item
getItem game p = field game ! p

bounds :: Game -> (Pos, Pos)
bounds (Game {rows, columns}) = ((0, 0), (rows-1, columns-1))

random :: Int -> Int -> Int -> Pos -> Int -> Rand Game
random rows columns numMines start buffer =
  do let bounds    = ((0, 0), (rows-1, columns-1))
     let positions = [p | p <- range bounds, not (isClose start p)]
     mines <- randomSubset numMines positions

     return $ Game { rows    = rows
                   , columns = columns
                   , mines   = length mines
                   , field   = mkMineField bounds mines
                   }

  where isClose (si, sj) (i, j) =
          abs (si - i) <= buffer && abs (sj - j) <= buffer

mkMineField :: (Pos, Pos) -> [Pos] -> MineField
mkMineField bounds mines = listArray bounds $ [item p | p <- range bounds]
  where mineMap = accumArray (||) False bounds [(p, True) | p <- mines]

        neighbors (i, j) = [p | di <- [-1, 0, 1],
                                dj <- [-1, 0, 1],
                                let p = (i + di, j + dj),
                                p /= (i, j),
                                inRange bounds p]

        hasMine = (mineMap !)
        item p | hasMine p = Mine
               | otherwise = Empty (length $ filter hasMine (neighbors p))
