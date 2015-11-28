{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Player.Smart
       (
         newPlayer
       ) where

import Data.Ratio (Ratio, (%))

import Draw (dimRect)
import Game (Pos)
import Player.API (Player, openEmpty, boxDraw, prompt)

import Player.Smart.Cells

type Prob = Ratio Int

instance CellValue Prob where
  merge   = max
  isFinal = (== 1)

test :: IO (Prob, Prob, Prob, Prob)
test =
  do minesLeft   <- ioCell 10
     numUnopened <- ioCell 100

     prob <- computedCell ((%) <$> v minesLeft <*> v numUnopened)

     v1 <- getValue prob

     setValue numUnopened 90

     v2 <- getValue prob

     addFormula prob (pure 1)

     v3 <- getValue prob

     setValue minesLeft 9
     setValue numUnopened 89

     v4 <- getValue prob

     return (v1, v2, v3, v4)

newPlayer :: Pos -> Player ()
newPlayer start = dim start >> dim (0, 0) >> prompt >> openEmpty start >> return ()
  where dim p = boxDraw p (dimRect 0.5 (0,0,1,1))
