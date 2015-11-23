module Colors
       (
         Color
       , dimgrey
       , grey
       , black
       , red
       ) where

import Data.Colour (Colour)
import Data.Colour.SRGB (sRGB24show)

import qualified Data.Colour.Names as Names

import Data.Text (Text, pack)

type Color = Text

showColor :: Colour Double -> Text
showColor = pack . sRGB24show

dimgrey :: Color
dimgrey = showColor Names.dimgrey

grey :: Color
grey = showColor Names.grey

black :: Color
black = showColor Names.black

red :: Color
red = showColor Names.red
