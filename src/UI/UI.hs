module UI.UI
  ( UI(UI, playerName, stats, game)
  , DeviceContext
  , Draw
  , display
  , drawUI
  , drawMsg
  , drawError
  , drawPosInfo
  , waitKeypress
  , runUI
  ) where

import Control.Concurrent.Async (concurrently_)
import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Control.Monad.Extra (ifM)
import Data.Ix (range)
import Graphics.Blank
  ( DeviceContext
  , Event(Event, eType, eWhich)
  , blankCanvas
  , events
  , flush
  , send
  , wait
  )
import Web.Browser (openBrowser)

import Game (Item(Empty, Mine), Game, Pos)
import qualified Game as Game
import PlayStats (PlayStats, numPlayed, numWon)

import UI.Colors
  ( Color
  , black
  , blue
  , brown
  , darkred
  , dimgrey
  , green
  , grey
  , khaki
  , lightgrey
  , purple
  , red
  )
import UI.Draw
  ( Draw
  , Rect
  , (|||)
  , aspectRatio
  , dimRect
  , drawText
  , fill
  , fillCircle
  , fillRect
  , restrict
  , runDraw
  , setFillColor
  , setFont
  , setLineWidth
  , setStrokeColor
  , stroke
  , strokeLine
  )

data UI =
  UI
    { playerName :: String
    , stats :: PlayStats
    , game :: Game
    }

display :: DeviceContext -> Draw () -> IO ()
display context drawing = send context (runDraw context drawing)

drawUI :: UI -> Draw ()
drawUI (UI {playerName, stats, game}) = do
  setFillColor dimgrey
  fillRect (0, 0, 1, 1)
  withBoard game (drawBoard game)
  drawPlayInfo game stats
  drawPlayerName playerName
  maybeDrawErrorBox

  where
    maybeDrawErrorBox
      | Just (p, item) <- Game.errorItem game = drawErrorBox game p item
      | otherwise = return ()

drawPosInfo :: Game -> [(Pos, String)] -> Draw ()
drawPosInfo game ps = do
  setFillColor black
  setStrokeColor black

  forM_ ps $ \(p, info) ->
    withBoard game $
    withBox game p $ do
      setFont "monospace" 0.4
      drawText info (0.5, 0.5)

drawMsg :: String -> Draw ()
drawMsg = drawMsgWithColor lightgrey

drawError :: String -> Draw ()
drawError = drawMsgWithColor red

waitKeypress :: DeviceContext -> IO ()
waitKeypress context = do
  ev <- wait context
  if likeEvent ev
    then flush context >> return ()
    else waitKeypress context
  where
    likeEvent (Event {eType, eWhich})
      | eType == "mousedown" = True
      | eType == "keydown"   = eWhich == Just 32 -- space
      | otherwise            = error "can't happen"

runUI :: (DeviceContext -> IO ()) -> IO ()
runUI loop =
  concurrently_
    waitAndOpenURL
    (blankCanvas settings {events = ["keydown", "mousedown"]} loop)
  where
    settings = fromIntegral port
    port = 3000 :: Int
    url = "http://127.0.0.1:" ++ show port ++ "/"

    waitAndOpenURL
      -- this is ugly, but there's no other way to interject into the
      -- web-server startup to know that the listening port was bound
      = threadDelay (100 * 1e3) >> openURL

    openURL =
      ifM
        (openBrowser url)
        (putStrLn "The UI has been opened in your browser")
        (putStrLn $ "To open the UI go to " ++ url)

-- internal
boardRect :: Game -> Draw Rect
boardRect game = do
  aspect <- aspectRatio

  let columns = fromIntegral (Game.numColumns game)
  let rows    = fromIntegral (Game.numRows game)

  let boxSide = min (aspect / columns) (1 / rows)

  let w = (columns * boxSide) / aspect
  let h = rows * boxSide
  let x = (1 - w) / 2
  let y = (1 - h) / 2

  return (x, y, w, h)

drawBoard :: Game -> Draw ()
drawBoard game = sequence_ [drawBox game p | p <- range (Game.bounds game)]

withBox :: Game -> Pos -> Draw a -> Draw a
withBox game (i, j) = restrict rect
  where
    w = 1 / fromIntegral (Game.numColumns game)
    h = 1 / fromIntegral (Game.numRows game)
    x = w * fromIntegral j
    y = h * fromIntegral i

    rect = (x, y, w, h)

drawBox :: Game -> Pos -> Draw ()
drawBox game p = withBox game p (draw maybeItem)
  where
    maybeItem = Game.item game p

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

  where
    draw Mine      = drawMine
    draw (Empty m) = drawEmpty m

drawEmpty :: Int -> Draw ()
drawEmpty 0     = return ()
drawEmpty mines = do
  setFont "monospace" 0.7
  setStrokeColor dimgrey
  setFillColor (color mines)
  drawText (show mines) (0.5, 0.5)

  where
    color 1 = blue
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

  restrict (0.1, 0.1, 0.8, 0.8) $ do
    fillCircle (0.1, 0.1, 0.8, 0.8)

    strokeLine (0.5, 0.025) (0.5, 0.975)
    strokeLine (0.025, 0.5) (0.975, 0.5)

    strokeLine (0.15, 0.15) (0.85, 0.85)
    strokeLine (0.15, 0.85) (0.85, 0.15)

    setStrokeColor grey
    setFillColor grey
    fillCircle (0.2, 0.2, 0.3, 0.3)

margins :: Rect
margins = (0.1, 0.1, 0.8, 0.8)

withBoard :: Game -> Draw () -> Draw ()
withBoard game drawing =
  restrict margins $ do
    rect <- boardRect game
    restrict rect drawing

drawPlayInfo :: Game -> PlayStats -> Draw ()
drawPlayInfo game stats = do
  setStrokeColor black
  setFillColor black

  restrict rect $ drawStats ||| drawNumMines

  where
    (mx, my, mw, _) = margins
    rect = (mx, 0, mw, my)
    frac n d = show n ++ "/" ++ show d
    mines = frac (Game.numMinesMarked game) (Game.numMines game)

    drawNumMines = do
      setFont "monospace" 0.4
      drawText ("Mines: " ++ mines) (0.5, 0.5)

    won = numWon stats
    total = numPlayed stats

    drawStats = do
      setFont "monospace" 0.4
      drawText ("Games won: " ++ frac won total) (0.5, 0.5)

drawPlayerName :: String -> Draw ()
drawPlayerName player =
  restrict rect $ do
    setFont "monospace" 0.4
    setStrokeColor black
    setFillColor black

    drawText ("Player: " ++ player) (0.5, 0.5)
  where
    (mx, my, mw, mh) = margins
    rect = (mx, my + mh, mw, 1 - my - mh)

drawMsgWithColor :: Color -> String -> Draw ()
drawMsgWithColor color msg = do
  dimRect 0.8 (0, 0, 1, 1)
  restrict margins $ do
    setFont "monospace" 0.1
    setStrokeColor color
    setFillColor color

    drawText msg (0.5, 0.5)

drawX :: Draw ()
drawX =
  restrict (0.1, 0.1, 0.8, 0.8) $ do
    setLineWidth 0.1
    setStrokeColor red
    strokeLine (0, 0) (1, 1)
    strokeLine (1, 0) (0, 1)

drawErrorBox :: Game -> Pos -> Item -> Draw ()
drawErrorBox game p item =
  withBoard game $ withBox game p (drawOpenBox item >> drawX)
