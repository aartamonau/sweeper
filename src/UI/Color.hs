module UI.Color
  ( Color
  , dimgrey
  , grey
  , black
  , red
  , blue
  , green
  , khaki
  , purple
  , darkred
  , brown
  , lightgrey
  , darkgrey
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

blue :: Color
blue = showColor Names.blue

green :: Color
green = showColor Names.green

khaki :: Color
khaki = showColor Names.khaki

purple :: Color
purple = showColor Names.purple

darkred :: Color
darkred = showColor Names.darkred

brown :: Color
brown = showColor Names.brown

lightgrey :: Color
lightgrey = showColor Names.lightgrey

darkgrey :: Color
darkgrey = showColor Names.darkgrey
