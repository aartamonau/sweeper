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
import qualified CheaterPlayer (newPlayer)

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
drawBoard play = sequence_ [drawBox play p | p <- range (playBounds play)]

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
  setLineWidth 0.075
  setStrokeColor black
  setFillColor grey
  fill
  stroke

drawOpenBox :: Item -> Draw ()
drawOpenBox item = do
  setLineWidth 0.05
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
  setLineWidth 0.125

  restrict (0.1, 0.1, 0.8, 0.8) $
    do fillCircle (0.1, 0.1, 0.8, 0.8)

       strokeLine (0.5, 0.025) (0.5, 0.975)
       strokeLine (0.025, 0.5) (0.975, 0.5)

       strokeLine (0.15, 0.15) (0.85, 0.85)
       strokeLine (0.15, 0.85) (0.85, 0.15)

       setStrokeColor grey
       setFillColor grey
       fillCircle (0.2, 0.2, 0.3, 0.3)

margins :: Rect
margins = (0.1, 0.1, 0.8, 0.8)

withBoard :: Play -> Draw () -> Draw ()
withBoard play drawing =
  restrict margins $
    do rect <- boardRect play
       restrict rect drawing

drawPlay :: Play -> Draw ()
drawPlay play = do
  setFillColor dimgrey
  fillRect (0, 0, 1, 1)
  withBoard play (drawBoard play)
  drawPlayInfo play

drawPlayInfo :: Play -> Draw ()
drawPlayInfo play =
  restrict rect $
    do setFont "monospace" 0.4
       setStrokeColor black
       setFillColor black

       drawText ("Mines left: " ++ show (minesLeft play)) (0.5, 0.5)
  where (mx, my, mw, _) = margins
        rect            = (mx, 0, mw, my)

drawMsg :: Color -> String -> Draw ()
drawMsg color msg =
  do dimRect 0.8 (0, 0, 1, 1)
     restrict margins $
       do setFont "monospace" 0.1
          setStrokeColor color
          setFillColor color

          drawText msg (0.5, 0.5)

drawError :: String -> Draw ()
drawError = drawMsg red

drawWinMsg :: String -> Draw ()
drawWinMsg = drawMsg lightgrey

drawX :: Draw ()
drawX =
  restrict (0.1, 0.1, 0.8, 0.8) $
    do setLineWidth 0.1
       setStrokeColor red
       strokeLine (0, 0) (1, 1)
       strokeLine (1, 0) (0, 1)

drawErrorBox :: Play -> Pos -> Item -> Draw ()
drawErrorBox play p item =
  withBoard play $ withBox play p (drawOpenBox item >> drawX)

drawErrorPlay :: Game -> Play -> Pos -> Draw ()
drawErrorPlay game play p = drawPlay play >> drawErrorBox play p item
  where item = gameItem game p

display :: DeviceContext -> Draw () -> IO ()
display context drawing = send context (runDraw context drawing)

main :: IO ()
main = do
  putStrLn "Go to http://127.0.0.1:3000/"

  blankCanvas 3000 {events = ["keydown", "mousedown"]} loop

loop :: DeviceContext -> IO ()
loop context =
  do let cols  = 10
     let rows  = 10
     let mines = 10
     let start = (rows `div` 2, cols `div` 2)

     game <- randomGame rows cols mines start

     let play   = newPlay game
     let player = CheaterPlayer.newPlayer game start
     -- let player = DummyPlayer.newPlayer start

     loopGame game play player context

loopGame :: Game -> Play -> Player () -> DeviceContext -> IO ()
loopGame game play player context =
  do display context (drawPlay play)
     waitEvent
     loopPlayer play player successCont errorCont surrenderCont

  where waitEvent =
          do ev <- wait context
             if likeEvent ev
               then flush context >> return ()
               else waitEvent

        likeEvent (Event {eType, eWhich})
          | eType == "mousedown" = True
          | eType == "keydown"   = eWhich == Just 32 -- space
          | otherwise            = error "can't happen"

        surrenderCont =
          display context (drawError "Player surrenders") >> loop context

        errorCont (p, msg) =
          do display context (drawErrorPlay game play p >> drawError msg)
             waitEvent
             loop context

        successCont (play', player')
          | anyMinesLeft play' = loopGame game play' player' context
          | otherwise          =
              do display context (drawPlay play' >> drawWinMsg "Player wins")
                 waitEvent
                 loop context

type Cont r = r -> IO ()

type SurrenderCont = IO ()
type ErrorCont     = Cont (Pos, String)
type SuccessCont   = Cont (Play, Player ())

loopPlayer :: Play -> Player () ->
              SuccessCont -> ErrorCont -> SurrenderCont -> IO ()
loopPlayer play player successCont errorCont surrender =
  do step <- runFreeT player
     case step of
      Pure _                    -> surrender
      Free (OpenEmpty p k)      -> handleOpenEmpty p k (openEmpty play p)
      Free (OpenMine p player') -> handleOpenMine p player' (openMine play p)
      Free (GetPlay k)          ->
        loopPlayer play (k play) successCont errorCont surrender

  where handleOpenEmpty p _ (Left err) =
          error p ("openEmpty errored: (" ++ show p ++ ") " ++ show err)
        handleOpenEmpty _ k (Right (r, play)) =
          success play (k r)

        handleOpenMine p _ (Left err) =
          error p ("openMine errored: (" ++ show p ++ ")" ++  show err)
        handleOpenMine _ player (Right play) =
          success play player

        success = curry successCont
        error   = curry errorCont
