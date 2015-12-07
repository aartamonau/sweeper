{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RecordWildCards #-}

module Mode.UI
       (
         run
       ) where

import Control.Concurrent (threadDelay)

import CmdArgs (Cfg,
                player, interactive, delay,
                fieldSpec, startMove, buffer)
import Game (randomGame)
import Play (Play,
             PlayError(ErrorFired, ErrorKilled, ErrorNoChange),
             newPlay, markMine, openEmpty, isWon)
import Player (Player(name, strategy),
               Strategy,
               Move(GetPlay, PosInfo, OpenEmpty, MarkMine),
               FreeF(Pure, Free),
               runFreeT)
import PlayStats (PlayStats,
                  incWon, incLost, incStalled)

import Mode.UI.UI (Draw, DeviceContext,
                   runUI, display,
                   waitKeypress,
                   drawMsg, drawError, drawPlay, drawPosInfo)

data Ctx = Ctx { ctxCfg       :: Cfg
               , ctxStats     :: PlayStats
               , ctxDeviceCtx :: DeviceContext
               }

run :: Cfg -> IO ()
run = runUI . enterLoop

drawUI :: (?ctx :: Ctx) => Play -> Draw ()
drawUI play = drawPlay play ctxStats (name $ player ctxCfg)
  where Ctx {..} = ?ctx

draw :: (?ctx :: Ctx) => Draw () -> IO ()
draw d = display context d >> wait context
  where Ctx {ctxDeviceCtx = context} = ?ctx

wait :: (?ctx :: Ctx) => DeviceContext -> IO ()
wait context | interactive cfg = waitKeypress context
             | otherwise       = threadDelay (1000 * delay cfg)

  where cfg = ctxCfg ?ctx

enterLoop :: Cfg -> DeviceContext -> IO ()
enterLoop cfg deviceCtx = let ?ctx = ctx in loop
  where ctx = Ctx { ctxCfg       = cfg
                  , ctxStats     = mempty
                  , ctxDeviceCtx = deviceCtx
                  }

loop :: (?ctx :: Ctx) => IO ()
loop =
  do let cfg = ctxCfg ?ctx

     let (rows, cols, mines) = fieldSpec cfg
     let start               = startMove cfg
     let buf                 = buffer cfg

     game <- randomGame rows cols mines start buf
     loopGame (newPlay game) (strategy (player cfg) start)

loopGame :: (?ctx :: Ctx) => Play -> Strategy () -> IO ()
loopGame play strategy =
  do draw (drawUI play)
     loopStrategy play strategy

loopStrategy :: (?ctx :: Ctx) => Play -> Strategy () -> IO ()
loopStrategy play strategy =
  do step <- runFreeT strategy
     case step of
      Pure _                      -> surrender
      Free (PosInfo ps strategy') -> handlePosInfo ps strategy'
      Free (OpenEmpty p k)        -> handleOpenEmpty k (openEmpty play p)
      Free (MarkMine p strategy') -> handleMarkMine strategy' (markMine play p)
      Free (GetPlay k)            -> nextStep (k play)

  where handlePosInfo ps strategy =
          do draw (drawPosInfo play ps)
             nextStep strategy

        handleOpenEmpty k (play, Left err) = handleError play err (k [])
        handleOpenEmpty k (play, Right r)  = success play (k r)

        handleMarkMine strategy (play, Left err) = handleError play err strategy
        handleMarkMine strategy (play, Right ()) = success play strategy

        restart :: (?ctx :: Ctx) => (PlayStats -> PlayStats) -> IO ()
        restart inc = let ?ctx = ?ctx {ctxStats = inc ctxStats} in loop
          where Ctx {..} = ?ctx

        continue play strategy = loopGame play strategy
        nextStep strategy      = loopStrategy play strategy

        surrender =
          do draw (drawUI play >> drawError "Player surrenders")
             restart incStalled

        handleError _ ErrorNoChange strategy = nextStep strategy
        handleError play err _               =
          do draw (drawUI play >> drawError (describeError err))
             restart incLost

        describeError ErrorKilled = "Player explodes on a mine"
        describeError ErrorFired  = "Player is fired due to incompetence"
        describeError _           = error "can't happen"

        success play strategy
          | isWon play =
              do draw (drawUI play >> drawMsg "Player wins")
                 restart incWon
          | otherwise  = continue play strategy
