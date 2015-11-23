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

type Pos = (Int, Int)
type MineField = Array Pos Item

data Item = Mine
          | Empty Int
          deriving Show

gameItem :: Game -> Pos -> Item
gameItem game p = field game ! p

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
    sequence_ [drawBox game p >> drawBoxItem game p |
               p <- range ((0, 0), (rows-1, columns-1))]

withBox :: Game -> Pos -> Draw a -> Draw a
withBox (Game {..}) (i, j) = restrict rect
  where w = 1 / fromIntegral columns
        h = 1 / fromIntegral rows
        x = w * fromIntegral j
        y = h * fromIntegral i

        rect = (x, y, w, h)

drawBox :: Game -> Pos -> Draw ()
drawBox game p = withBox game p $ do
  setStrokeColor black
  setFillColor grey
  fill
  stroke

drawBoxItem :: Game -> Pos -> Draw ()
drawBoxItem game p = withBox game p (draw item)
  where item = gameItem game p

        draw Mine      = drawMine
        draw (Empty m) = drawEmpty m

drawEmpty :: Int -> Draw ()
drawEmpty 0     = return ()
drawEmpty mines = do
  setFont "monospace" 0.4
  setFillColor (color mines)
  fillText (show mines) (0.5, 0.5)

  where color 1 = blue
        color 2 = green
        color 3 = khaki
        color 4 = purple
        color 5 = red
        color 6 = darkred
        color 7 = brown
        color 8 = black
        color _ = error "can't happen"

drawMine :: Draw ()
drawMine = do
  setStrokeColor black
  setFillColor black
  fillCircle (0.2, 0.2, 0.6, 0.6)

  setStrokeColor dimgrey
  setFillColor dimgrey
  fillCircle (0.3, 0.3, 0.2, 0.2)


drawGame :: Game -> Draw ()
drawGame game@(Game {..}) = do
  setFillColor dimgrey
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

genGame :: Int -> Int -> Int -> Pos -> IO Game
genGame rows columns numMines start = do
  let bounds    = ((0, 0), (rows-1, columns-1))
  let positions = [p | p <- range bounds, p /= start]
  mines <- take numMines <$> shuffleM positions

  return $ Game { rows    = rows
                , columns = columns
                , field   = mkMineField bounds mines
                }

mkMineField :: (Pos, Pos)  -> [Pos] -> MineField
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
