{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent (threadDelay)

import Game
import Play
import Player
import PlayStats

import CmdArgs
import UI

main :: IO ()
main = runWithCfg $ \cfg -> runUI (enterLoop cfg)

data Ctx = Ctx { ctxCfg   :: Cfg
               , ctxStats :: PlayStats
               }

drawUI :: (?ctx :: Ctx) => Play -> Draw ()
drawUI play = drawPlay play ctxStats (name $ player ctxCfg)
  where Ctx {..} = ?ctx

draw :: (?ctx :: Ctx) => DeviceContext -> Draw () -> IO ()
draw context d = display context d >> wait context

wait :: (?ctx :: Ctx) => DeviceContext -> IO ()
wait context | interactive cfg = waitKeypress context
             | otherwise       = threadDelay (1000 * delay cfg)

  where cfg = ctxCfg ?ctx

enterLoop :: Cfg -> DeviceContext -> IO ()
enterLoop cfg = let ?ctx = ctx in loop
  where ctx = Ctx { ctxCfg   = cfg
                  , ctxStats = makeStats
                  }

loop :: (?ctx :: Ctx) => DeviceContext -> IO ()
loop context =
  do let cfg = ctxCfg ?ctx

     let (rows, cols, mines) = fieldSpec cfg
     let start               = startMove cfg
     let buf                 = buffer cfg

     game <- randomGame rows cols mines start buf
     loopGame (newPlay game) (strategy (player cfg) start) context

loopGame :: (?ctx :: Ctx) => Play -> Strategy () -> DeviceContext -> IO ()
loopGame play strategy context =
  do draw context (drawUI play)
     loopStrategy play strategy context

loopStrategy :: (?ctx :: Ctx) => Play -> Strategy () -> DeviceContext -> IO ()
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

        restart :: (?ctx :: Ctx) => (PlayStats -> PlayStats) -> IO ()
        restart inc = let ?ctx = ?ctx {ctxStats = inc ctxStats} in loop context
          where Ctx {..} = ?ctx

        continue play strategy = loopGame play strategy context
        nextStep strategy      = loopStrategy play strategy context

        surrender =
          do draw context (drawUI play
                           >> drawError "Player surrenders")
             restart incStalled

        handleError _ ErrorNoChange strategy = nextStep strategy
        handleError play err _               =
          do draw context (drawUI play
                           >> drawError (describeError err))
             restart incLost

        describeError ErrorKilled = "Player explodes on a mine"
        describeError ErrorFired  = "Player is fired due to incompetence"
        describeError _           = error "can't happen"

        success play strategy
          | isWon play =
              do draw context (drawUI play
                               >> drawMsg "Player wins")
                 restart incWon
          | otherwise  = continue play strategy
