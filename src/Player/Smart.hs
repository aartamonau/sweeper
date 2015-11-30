{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Player.Smart
       (
         newPlayer
       ) where

import Control.Arrow (second)
import Control.Monad (forM, replicateM)

import Data.Array (Array, listArray, assocs, rangeSize)
import Data.Ratio (Ratio, (%), numerator, denominator)

import Colors (black)
import Draw (drawText, setFillColor, setFont)
import Game (Pos)
import Play (Play, minesLeft, isOpened, playBounds)
import Player.API (Player, Strategy,
                   makePlayer,
                   openEmpty, draw, getPlay, io)

import Player.Smart.Cells

type Prob = Ratio Int

instance CellValue Prob where
  merge   = max
  isFinal = (== 1)

data State = State { minesLeftCell   :: Cell 'IOCell Int
                   , numUnopenedCell :: Cell 'IOCell Int

                   , fieldCells :: Array Pos (Cell 'ComputedCell Prob)
                   }

makeState :: Play -> Strategy State
makeState play = io $
  do minesLeftCell   <- ioCell (minesLeft play)
     numUnopenedCell <- ioCell size

     let formula = (%) <$> v minesLeftCell <*> v numUnopenedCell
     fieldCells <- listArray bounds <$> replicateM size (computedCell formula)

     return $ State { minesLeftCell, numUnopenedCell, fieldCells }
  where bounds = playBounds play
        size   = rangeSize bounds

drawProbs :: Play -> State -> Strategy ()
drawProbs play (State {fieldCells}) =
  do let unopened = [(p, cell) | (p, cell) <- assocs fieldCells,
                                 not (isOpened play p)]

     probs <- forM unopened $ \(p, cell) -> (p,) <$> io (getValue cell)
     draw $ map (second drawProb) probs
  where drawProb prob =
          do let num   = numerator prob
             let denom = denominator prob
             let text  = show num ++ "/" ++ show denom

             setFillColor black
             setFont "monospace" 0.2

             drawText text (0.5, 0.5)

newPlayer :: Pos -> Player
newPlayer start = makePlayer "smart" (newStrategy start)

newStrategy :: Pos -> Strategy ()
newStrategy start =
  do play  <- getPlay
     state <- makeState play

     drawProbs play state

     _ <- openEmpty start


     play' <- getPlay
     drawProbs play' state

     return ()
