{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Player.Constraints
       (
         player
       ) where

import Control.Arrow (second)
import Control.Monad (forM, replicateM)

import Data.Array (Array, listArray, assocs, rangeSize)
import Data.Ratio (Ratio, (%), numerator, denominator)

import Game (Pos)
import Play (Play, isOpened, playBounds, playNumMines)
import Player.API (Player, Strategy,
                   makePlayer,
                   openEmpty, posInfo, getPlay, st)

import Player.Constraints.Cells

type Prob = Ratio Int

instance CellValue Prob where
  merge   = max
  isFinal = (== 1)

data State = State { minesLeftCell   :: Cell 'IOCell Int
                   , numUnopenedCell :: Cell 'IOCell Int

                   , fieldCells :: Array Pos (Cell 'ComputedCell Prob)
                   }

makeState :: Play -> Strategy State
makeState play = st $
  do minesLeftCell   <- ioCell mines
     numUnopenedCell <- ioCell size

     let formula = (%) <$> v minesLeftCell <*> v numUnopenedCell
     fieldCells <- listArray bounds <$> replicateM size (computedCell formula)

     return $ State { minesLeftCell, numUnopenedCell, fieldCells }
  where mines  = playNumMines play
        bounds = playBounds play
        size   = rangeSize bounds

drawProbs :: Play -> State -> Strategy ()
drawProbs play (State {fieldCells}) =
  do let unopened = [(p, cell) | (p, cell) <- assocs fieldCells,
                                 not (isOpened play p)]

     probs <- forM unopened $ \(p, cell) -> (p,) <$> st (getValue cell)
     posInfo $ map (second showProb) probs
  where showProb prob = show (numerator prob) ++ "/" ++ show (denominator prob)

player :: Player
player = makePlayer "constraints" strategy

strategy :: Pos -> Strategy ()
strategy start =
  do play  <- getPlay
     state <- makeState play

     drawProbs play state

     _ <- openEmpty start

     play' <- getPlay
     drawProbs play' state

     return ()
