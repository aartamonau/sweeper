{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Array (Array, (!), inRange, range, accumArray, listArray)
import Graphics.Blank (DeviceContext, blankCanvas, send)

import System.Random.Shuffle (shuffleM)

import Colors
import Draw

data Game =
  Game { rows    :: Int
       , columns :: Int
       , field   :: MineField
       }
  deriving Show

type MineField = Array (Int, Int) Item

data Item = Mine
          | Empty Int
          deriving Show

bgColor :: Color
bgColor = dimgrey

boxColor :: Color
boxColor = grey

strokeColor :: Color
strokeColor = black

boardRect :: Game -> Draw Rect
boardRect (Game {..}) = do
  aspect <- aspectRatio

  let columns' = fromIntegral columns
  let rows'    = fromIntegral rows

  let boxSide = min (aspect / columns') (1 / rows')

  let w = (columns' * boxSide) / aspect
  let h = rows' * boxSide
  let x = (1 - w) / 2
  let y = (1 - h) / 2

  return (x, y, w, h)

drawBoard :: Game -> Draw ()
drawBoard game@(Game {..}) = do
  rect <- boardRect game

  restrict rect $
    sequence_ [drawBox game p | p <- range ((0, 0), (rows-1, columns-1))]

withBox :: Game -> (Int, Int) -> Draw a -> Draw a
withBox (Game {..}) (i, j) = restrict rect
  where w = 1 / fromIntegral columns
        h = 1 / fromIntegral rows
        x = w * fromIntegral j
        y = h * fromIntegral i

        rect = (x, y, w, h)

drawBox :: Game -> (Int, Int) -> Draw ()
drawBox game p = withBox game p $ do
  setFillColor boxColor
  fill

  setStrokeColor strokeColor
  stroke

drawGame :: Game -> Draw ()
drawGame game@(Game {..}) = do
  setFillColor bgColor
  fillRect (0, 0, 1, 1)
  restrict (0.1, 0.1, 0.8, 0.8) $ drawBoard game

main :: IO ()
main = do
  putStrLn "Go to http://127.0.0.1:3000/"

  let cols  = 16
  let rows  = 30
  let mines = 99

  game <- genGame cols rows mines (rows `div` 2, cols `div` 2)
  blankCanvas 3000 (loop game)

loop :: Game -> DeviceContext -> IO ()
loop game context = send context $ runDraw context $ drawGame game

genGame :: Int -> Int -> Int -> (Int, Int) -> IO Game
genGame rows columns numMines start = do
  let bounds    = ((0, 0), (rows-1, columns-1))
  let positions = [p | p <- range bounds, p /= start]
  mines <- take numMines <$> shuffleM positions

  return $ Game { rows    = rows
                , columns = columns
                , field   = mkMineField bounds mines
                }

mkMineField :: ((Int, Int), (Int, Int))  -> [(Int, Int)] -> MineField
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
