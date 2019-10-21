{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Mode.UI.UI
       (
         UI (UI, playerName, stats, play)
       , DeviceContext
       , Draw
       , display
       , drawUI
       , drawMsg
       , drawError
       , drawPosInfo
       , waitKeypress
       , runUI
       )
       where

import Control.Monad (forM_)
import Data.Ix (range)
import Graphics.Blank (DeviceContext, blankCanvas, send,
                       Event (Event, eType, eWhich),
                       events, wait, flush)

import Play (Play(numMinesMarked),
             Pos, Item(Mine, Empty),
             playRows, playColumns, playBounds,
             playItem, playNumMines, errorItem)
import PlayStats (PlayStats, numWon, numPlayed)

import Mode.UI.Colors (Color,
                       black, red, dimgrey, grey, lightgrey,
                       blue, green, khaki, purple, darkred, brown)
import Mode.UI.Draw (Draw, Rect,
                     (|||),
                     runDraw, restrict, aspectRatio,
                     setFont, setFillColor, setStrokeColor,
                     drawText, fillRect, setLineWidth,
                     fill, stroke, fillCircle, strokeLine, dimRect)

data UI =
  UI { playerName :: String
     , stats      :: !PlayStats
     , play       :: !Play
     }

display :: DeviceContext -> Draw () -> IO ()
display context drawing = send context (runDraw context drawing)

drawUI :: UI -> Draw ()
drawUI (UI {playerName, stats, play}) =
  do setFillColor dimgrey
     fillRect (0, 0, 1, 1)
     withBoard play (drawBoard play)
     drawPlayInfo play stats
     drawPlayerName playerName
     maybeDrawErrorBox

  where maybeDrawErrorBox
          | Just (p, item) <- errorItem play = drawErrorBox play p item
          | otherwise                        = return ()

drawPosInfo :: Play -> [(Pos, String)] -> Draw ()
drawPosInfo play ps =
  do setFillColor black
     setStrokeColor black

     forM_ ps $ \(p, info) ->
       withBoard play $ withBox play p $
         do setFont "monospace" 0.4
            drawText info (0.5, 0.5)

drawMsg :: String -> Draw ()
drawMsg = drawMsgWithColor lightgrey

drawError :: String -> Draw ()
drawError = drawMsgWithColor red

waitKeypress :: DeviceContext -> IO ()
waitKeypress context =
  do ev <- wait context
     if likeEvent ev
       then flush context >> return ()
       else waitKeypress context
  where likeEvent (Event {..})
          | eType == "mousedown" = True
          | eType == "keydown"   = eWhich == Just 32 -- space
          | otherwise            = error "can't happen"

runUI :: (DeviceContext -> IO ()) -> IO ()
runUI loop =
  do putStrLn "To open the UI go to http://127.0.0.1:3000/"
     blankCanvas 3000 {events = ["keydown", "mousedown"]} loop

-- internal
boardRect :: Play -> Draw Rect
boardRect play =
  do aspect <- aspectRatio

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
drawClosedBox =
  do setLineWidth 0.075
     setStrokeColor black
     setFillColor grey
     fill
     stroke

drawOpenBox :: Item -> Draw ()
drawOpenBox item =
  do setLineWidth 0.05
     setStrokeColor black
     setFillColor lightgrey
     fill
     stroke
     draw item

  where draw Mine      = drawMine
        draw (Empty m) = drawEmpty m

drawEmpty :: Int -> Draw ()
drawEmpty 0     = return ()
drawEmpty mines =
  do setFont "monospace" 0.7
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
drawMine =
  do setStrokeColor black
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

drawPlayInfo :: Play -> PlayStats -> Draw ()
drawPlayInfo play stats =
  do setStrokeColor black
     setFillColor black

     restrict rect $ drawStats ||| drawNumMines
  where (mx, my, mw, _) = margins
        rect            = (mx, 0, mw, my)

        frac n d = show n ++ "/" ++ show d

        mines = frac (numMinesMarked play) (playNumMines play)

        drawNumMines =
          do setFont "monospace" 0.4
             drawText ("Mines: " ++ mines) (0.5, 0.5)

        won   = numWon stats
        total = numPlayed stats

        drawStats =
          do setFont "monospace" 0.4
             drawText ("Games won: " ++ frac won total) (0.5, 0.5)

drawPlayerName :: String -> Draw ()
drawPlayerName player =
  restrict rect $
    do setFont "monospace" 0.4
       setStrokeColor black
       setFillColor black

       drawText ("Player: " ++ player) (0.5, 0.5)
  where (mx, my, mw, mh) = margins
        rect             = (mx, my + mh, mw, 1 - my - mh)

drawMsgWithColor :: Color -> String -> Draw ()
drawMsgWithColor color msg =
  do dimRect 0.8 (0, 0, 1, 1)
     restrict margins $
       do setFont "monospace" 0.1
          setStrokeColor color
          setFillColor color

          drawText msg (0.5, 0.5)

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
