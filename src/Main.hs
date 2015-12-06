{-# LANGUAGE ImplicitParams #-}

module Main where

import Control.Concurrent (threadDelay)

import Game
import Play
import Player

import CmdArgs
import UI

main :: IO ()
main = runWithCfg $ \cfg -> runUI (enterLoop cfg)

drawUI :: (?cfg :: Cfg) => Play -> Draw ()
drawUI play = drawPlay play (name $ player ?cfg)

draw :: (?cfg :: Cfg) => DeviceContext -> Draw () -> IO ()
draw ctx d = display ctx d >> wait ctx

wait :: (?cfg :: Cfg) => DeviceContext -> IO ()
wait context | interactive ?cfg = waitKeypress context
             | otherwise        = threadDelay (1000 * delay ?cfg)

enterLoop :: Cfg -> DeviceContext -> IO ()
enterLoop cfg = let ?cfg = cfg in loop

loop :: (?cfg :: Cfg) => DeviceContext -> IO ()
loop context =
  do let (rows, cols, mines) = fieldSpec ?cfg
     let start               = startMove ?cfg
     let buf                 = buffer ?cfg

     game <- randomGame rows cols mines start buf
     loopGame (newPlay game) (strategy (player ?cfg) start) context

loopGame :: (?cfg :: Cfg) => Play -> Strategy () -> DeviceContext -> IO ()
loopGame play strategy context =
  do draw context (drawUI play)
     loopStrategy play strategy context

loopStrategy :: (?cfg :: Cfg) => Play -> Strategy () -> DeviceContext -> IO ()
loopStrategy play strategy context =
  do step <- runFreeT strategy
     case step of
      Pure _                      -> surrender
      Free (PosInfo ps strategy') -> handlePosInfo ps strategy'
      Free (OpenEmpty p k)        -> handleOpenEmpty k (openEmpty play p)
      Free (MarkMine p strategy') -> handleMarkMine strategy' (markMine play p)
      Free (GetPlay k)            -> nextStep (k play)

  where handlePosInfo ps strategy =
          do draw context (drawPosInfo play ps)
             nextStep strategy

        handleOpenEmpty k (play, Left err) = handleError play err (k [])
        handleOpenEmpty k (play, Right r)  = success play (k r)

        handleMarkMine strategy (play, Left err) = handleError play err strategy
        handleMarkMine strategy (play, Right ()) = success play strategy

        restart                = loop context
        continue play strategy = loopGame play strategy context
        nextStep strategy      = loopStrategy play strategy context

        surrender =
          do draw context (drawUI play
                           >> drawError "Player surrenders")
             restart

        handleError _ ErrorNoChange strategy = nextStep strategy
        handleError play err _               =
          do draw context (drawUI play
                           >> drawError (describeError err))
             restart

        describeError ErrorKilled = "Player explodes on a mine"
        describeError ErrorFired  = "Player is fired due to incompetence"
        describeError _           = error "can't happen"

        success play strategy
          | isWon play =
              do draw context (drawUI play
                               >> drawMsg "Player wins")
                 restart
          | otherwise  = continue play strategy
