module Draw
       (
         Draw
       , Point
       , Rect
       , runDraw
       , restrict
       , aspectRatio
       , setStrokeColor
       , setFillColor
       , strokeRect
       , fillRect
       , stroke
       , fill
       , setFont
       , drawText
       , fillCircle
       , setLineWidth
       , strokeLine
       ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, asks, local)

import Data.Text (pack)

import Graphics.Blank (Canvas, DeviceContext)
import qualified Graphics.Blank as Blank

import Colors (Color)

type Point = (Double, Double)
type Rect  = (Double, Double, Double, Double)

type Transform = Point -> Point

type Draw a = ReaderT Transform Canvas a

runDraw :: DeviceContext -> Draw a -> Canvas a
runDraw context drawing = runReaderT drawing trans
  where w = Blank.width context
        h = Blank.height context

        trans (x, y) = (x * w, y * h)

restrict :: Rect -> Draw a -> Draw a
restrict (rx, ry, rw, rh) = local (.trans)
  where trans (x, y) = (rx + x * rw, ry + y * rh)

aspectRatio :: Draw Double
aspectRatio = do
  (_, _, x, y) <- transRect (0, 0, 1, 1)
  return (x / y)

setStrokeColor :: Color -> Draw ()
setStrokeColor = lift . Blank.strokeStyle

setFillColor :: Color -> Draw ()
setFillColor = lift . Blank.fillStyle

transPoint :: Point -> Draw Point
transPoint p = asks ($ p)

transRect :: Rect -> Draw Rect
transRect (tx, ty, w, h) = do
  (tx', ty') <- transPoint (tx, ty)
  (bx', by') <- transPoint (tx + w, ty + h)

  return (tx', ty', bx' - tx', by' - ty')

liftCanvas :: (a -> Draw a) -> (a -> Canvas b) -> a -> Draw b
liftCanvas trans op x = trans x >>= lift . op

liftPoint :: (Point -> Canvas a) -> Point -> Draw a
liftPoint = liftCanvas transPoint

liftRect :: (Rect -> Canvas a) -> Rect -> Draw a
liftRect = liftCanvas transRect

strokeRect :: Rect -> Draw ()
strokeRect = liftRect Blank.strokeRect

fillRect :: Rect -> Draw ()
fillRect = liftRect Blank.fillRect

stroke :: Draw ()
stroke = strokeRect (0, 0, 1, 1)

fill :: Draw ()
fill = fillRect (0, 0, 1, 1)

setFont :: String -> Double -> Draw ()
setFont font sz = do
  (_, _, _, y) <- transRect (0, 0, 0, sz)

  let px = round y :: Int
  let fontSpec = pack $ show px ++ "px " ++ font
  lift $ Blank.font fontSpec

drawText :: String -> Point -> Draw ()
drawText text = liftPoint $ \(x, y) ->
  do Blank.textBaseline Blank.MiddleBaseline
     Blank.textAlign Blank.CenterAnchor
     Blank.fillText (pack text, x, y)
     Blank.strokeText (pack text, x, y)

fillCircle :: Rect -> Draw ()
fillCircle = liftRect $ \(x, y, w, h) ->
  do let rx = w / 2
     let ry = h / 2
     let cx = x + rx
     let cy = y + ry
     let r  = min rx ry

     Blank.beginPath ()
     Blank.arc (cx, cy, r, 0, 360, False)
     Blank.fill ()
     Blank.closePath ()

setLineWidth :: Double -> Draw ()
setLineWidth w = do
  (_, _, x, y) <- transRect (0, 0, w, w)
  lift $ Blank.lineWidth (min x y)

strokeLine :: Point -> Point -> Draw ()
strokeLine start end = do
  start' <- transPoint start
  end'   <- transPoint end

  lift $
    do Blank.beginPath ()
       Blank.moveTo start'
       Blank.lineTo end'
       Blank.stroke ()
       Blank.closePath ()
