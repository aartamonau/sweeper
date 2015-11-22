{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Blank

import Colors

data Game =
  Game { rows    :: Int
       , columns :: Int
       }
  deriving Show

data Rect =
  Rect { rectX :: Int
       , rectY :: Int
       , rectW :: Int
       , rectH :: Int
       }
  deriving Show

bgColor :: Color
bgColor = dimgrey

boxColor :: Color
boxColor = grey

strokeColor :: Color
strokeColor = black

drawBox :: Rect -> Int -> Int -> Int -> Canvas ()
drawBox (Rect {..}) w i j = do
  let boxX = rectX + w * j
  let boxY = rectY + w * i

  let boxW = fromIntegral w
  let box = (fromIntegral boxX, fromIntegral boxY, boxW, boxW)

  fillStyle boxColor
  fillRect box

  strokeStyle strokeColor
  strokeRect box

boxWidth :: Game -> Int -> Int -> Int
boxWidth (Game {..}) w h = min (w `div` columns) (h `div` rows)

boardRect :: Game -> Int -> Int -> Int -> Rect
boardRect (Game {..}) w h boxW = Rect boardX boardY boardW boardH
  where boardW = boxW * columns
        boardH = boxW * rows
        boardX = (w - boardW) `div` 2
        boardY = (h - boardH) `div` 2

drawGame :: Game -> DeviceContext -> Canvas ()
drawGame game@(Game {..}) context = do
  let w = width context
  let h = height context

  let w' = truncate w
  let h' = truncate h

  let boxW  = boxWidth game w' h'
  let board = boardRect game w' h' boxW

  fillStyle bgColor
  fillRect (0, 0, w, h)

  sequence_ [drawBox board boxW i j | i <- [0..rows - 1], j <- [0..columns-1]]

  return ()

main :: IO ()
main = do
  putStrLn "Go to http://127.0.0.1:3000/"
  blankCanvas 3000 loop

loop :: DeviceContext -> IO ()
loop context = send context $ drawGame (Game 16 30) context
