{-# LANGUAGE RecordWildCards #-}

module Game
       (
         Game (Game, rows, columns, field, mines)
       , MineField
       , Pos
       , Item (Mine, Empty)
       , randomGame
       , gameItem
       , gameBounds
       ) where

import Control.Monad.ST (ST, runST)
import Data.Array (Array, (!), inRange, range, accumArray, listArray)
import qualified Data.Array.MArray as MArr
import Data.Array.ST (STArray)
import System.Random.Mersenne (getStdRandom, randoms)

data Game =
  Game { rows    :: Int
       , columns :: Int
       , mines   :: Int
       , field   :: MineField
       }
  deriving Show

type Pos = (Int, Int)
type MineField = Array Pos Item

data Item = Mine
          | Empty Int
          deriving (Show, Eq)

gameItem :: Game -> Pos -> Item
gameItem game p = field game ! p

gameBounds :: Game -> (Pos, Pos)
gameBounds (Game {..}) = ((0, 0), (rows-1, columns-1))

randomGame :: Int -> Int -> Int -> Pos -> Int -> IO Game
randomGame rows columns numMines start buffer =
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

randomSubset :: Int -> [a] -> IO [a]
randomSubset k xs =
  do rs <- getStdRandom randoms
     return (doRandomSubset k xs rs)

doRandomSubset :: Int -> [a] -> [Int] -> [a]
doRandomSubset k xs rs
  | length init < k = xs
  | otherwise       =
      runST $
        do arr <- MArr.newListArray (0, k-1) init
           mapM_ (maybeSwap arr) $ zip3 [k..] rs rest
           MArr.getElems arr
  where (init, rest) = splitAt k xs

        maybeSwap :: STArray s Int a -> (Int, Int, a) -> ST s ()
        maybeSwap arr (i, r, x) | j < k     = MArr.writeArray arr j x
                                | otherwise = return ()
          where j = r `mod` i
