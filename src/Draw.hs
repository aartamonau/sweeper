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
       ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, asks, local)

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
