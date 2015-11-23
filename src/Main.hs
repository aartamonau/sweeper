{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Ix (range)
import Graphics.Blank (DeviceContext, blankCanvas, send,
                       Event (Event, eType, eWhich),
                       events, wait, flush)

import Colors
import Draw
import Game
import Play
import Player

import qualified DummyPlayer (newPlayer)

boardRect :: Play -> Draw Rect
boardRect play = do
  aspect <- aspectRatio

  let columns' = fromIntegral (playColumns play)
  let rows'    = fromIntegral (playRows play)

  let boxSide = min (aspect / columns') (1 / rows')

  let w = (columns' * boxSide) / aspect
  let h = rows' * boxSide
  let x = (1 - w) / 2
  let y = (1 - h) / 2

  return (x, y, w, h)

drawBoard :: Play -> Draw ()
drawBoard play = do
  rect <- boardRect play

  restrict rect $
    sequence_ [drawBox play p | p <- range (playBounds play)]

withBox :: Play -> Pos -> Draw a -> Draw a
withBox play (i, j) = restrict rect
  where w = 1 / fromIntegral (playColumns play)
        h = 1 / fromIntegral (playRows play)
        x = w * fromIntegral j
        y = h * fromIntegral i

        rect = (x, y, w, h)

drawBox :: Play -> Pos -> Draw ()
drawBox play p = withBox play p (draw maybeItem)
  where maybeItem = playItem play p

        draw Nothing     = drawClosedBox
        draw (Just item) = drawOpenBox item

drawClosedBox :: Draw ()
drawClosedBox = do
  setStrokeColor black
  setFillColor grey
  fill
  stroke

drawOpenBox :: Item -> Draw ()
drawOpenBox item = do
  setStrokeColor black
  setFillColor lightgrey
  fill
  stroke
  draw item

  where draw Mine      = drawMine
        draw (Empty m) = drawEmpty m

drawEmpty :: Int -> Draw ()
drawEmpty 0     = return ()
drawEmpty mines = do
  setFont "monospace" 0.7
  setStrokeColor dimgrey
  setFillColor (color mines)
  drawText (show mines) (0.5, 0.5)

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

  setStrokeColor grey
  setFillColor grey
  fillCircle (0.3, 0.3, 0.2, 0.2)

drawPlay :: Play -> Draw ()
drawPlay play = do
  setFillColor dimgrey
  fillRect (0, 0, 1, 1)
  restrict (0.1, 0.1, 0.8, 0.8) $ drawBoard play

main :: IO ()
main = do
  putStrLn "Go to http://127.0.0.1:3000/"

  blankCanvas 3000 {events = ["keydown", "mousedown"]} loop

loop :: DeviceContext -> IO ()
loop context =
  do let cols  = 30
     let rows  = 16
     let mines = 99
     let start = (rows `div` 2, cols `div` 2)

     game <- randomGame rows cols mines start

     let play   = newPlay game
     let player = DummyPlayer.newPlayer start

     loopGame play player context

loopGame :: Play -> (Player ()) -> DeviceContext -> IO ()
loopGame play player context =
  do draw
     waitEvent
     loopPlayer play player context

  where draw = send context $ runDraw context $ drawPlay play
        waitEvent =
          do ev <- wait context
             if likeEvent ev
               then flush context >> return ()
               else waitEvent

        likeEvent (Event {eType, eWhich})
          | eType == "mousedown" = True
          | eType == "keydown"   = eWhich == Just 32 -- space
          | otherwise            = error "can't happen"

loopPlayer :: Play -> (Player ()) -> DeviceContext -> IO ()
loopPlayer play player context =
  do step <- runFreeT player
     case step of
      Pure _ ->
        do putStrLn "player surrenderred; starting new game"
           loop context
      Free (OpenEmpty p k) ->
        handleOpenEmpty p k (openEmpty play p)
      Free (OpenMine p player') ->
        handleOpenMine p player' (openMine play p)
      Free (GetPlay k) ->
        loopPlayer play (k play) context

  where handleOpenEmpty p _ (Left err) =
          do putStrLn $ "openEmpty errored: (" ++ show p ++ ") " ++ show err
             loop context
        handleOpenEmpty _ k (Right (r, play)) =
          loopGame play (k r) context

        handleOpenMine p _ (Left err) =
          do putStrLn $ "openMine errored: (" ++ show p ++ ")" ++  show err
             loop context
        handleOpenMine _ player (Right play) =
          loopGame play player context
