module UI.Colors where

import Data.Colour (Colour)
import Data.Colour.SRGB (sRGB24show)

import qualified Data.Colour.Names as Names

import Data.Text (Text, pack)

type Color = Text

showColor :: Colour Double -> Text
showColor = pack . sRGB24show

dimgrey, grey, black, red  :: Color
blue, green, khaki, purple :: Color
darkred, brown, lightgrey  :: Color
[dimgrey, grey, black, red, blue, green,
 khaki, purple, darkred, brown, lightgrey] =
  map
    showColor
    [ Names.dimgrey
    , Names.grey
    , Names.black
    , Names.red
    , Names.blue
    , Names.green
    , Names.khaki
    , Names.purple
    , Names.darkred
    , Names.brown
    , Names.lightgrey
    ]
