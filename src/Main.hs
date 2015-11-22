{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Graphics.Blank (DeviceContext, blankCanvas, send)

import Colors
import Draw

data Game =
  Game { rows    :: Int
       , columns :: Int
       }
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
    sequence_ [drawBox game i j | i <- [0..rows - 1], j <- [0..columns-1]]

drawBox :: Game -> Int -> Int -> Draw ()
drawBox (Game {..}) i j = do
  setFillColor boxColor
  fillRect rect

  setStrokeColor strokeColor
  strokeRect rect

  where w = 1 / fromIntegral columns
        h = 1 / fromIntegral rows
        x = w * fromIntegral j
        y = h * fromIntegral i

        rect = (x, y, w, h)

drawGame :: Game -> Draw ()
drawGame game@(Game {..}) = do
  setFillColor bgColor
  fillRect (0, 0, 1, 1)
  restrict (0.1, 0.1, 0.8, 0.8) $ drawBoard game

main :: IO ()
main = do
  putStrLn "Go to http://127.0.0.1:3000/"
  blankCanvas 3000 loop

loop :: DeviceContext -> IO ()
loop context = send context $ runDraw context $ drawGame (Game 16 30)
