module Mode.UI
       (
         run
       ) where

import Control.Concurrent (threadDelay)

import CmdArgs (Cfg, UICfg,
                cfgPlayer, cfgStartMove, cfgMakeGen,
                uiInteractive, uiDelay)
import Play (Play,
             PlayError(ErrorFired, ErrorKilled, ErrorNoChange),
             newPlay, markMine, openEmpty, isWon)
import Player (Player(name, strategy),
               Strategy,
               Move(GetPlay, PosInfo, OpenEmpty, MarkMine),
               FreeF(Pure, Free),
               runStrategy)
import PlayStats (PlayStats,
                  incWon, incLost, incStalled)
import Rand (Gen)

import Mode.Common (randomGame)
import Mode.UI.UI (UI (UI, playerName, stats, play),
                   Draw, DeviceContext,
                   runUI, display,
                   waitKeypress,
                   drawMsg, drawError, drawUI, drawPosInfo)

data Ctx = Ctx { cfg           :: Cfg
               , uiCfg         :: UICfg
               , stats         :: PlayStats
               , deviceContext :: DeviceContext
               , rndGen        :: Gen
               }

run :: Cfg -> UICfg -> IO ()
run cfg = runUI . enterLoop cfg

draw :: Ctx -> Play -> Draw ()
draw (Ctx {stats, cfg}) play =
  drawUI $ UI { play, stats, playerName }
  where playerName = name $ cfgPlayer cfg

present :: Ctx -> Draw () -> IO ()
present ctx@(Ctx {deviceContext}) d =
  display deviceContext d >> wait ctx

wait :: Ctx -> IO ()
wait (Ctx {deviceContext, uiCfg})
  | uiInteractive uiCfg = waitKeypress deviceContext
  | otherwise           = threadDelay (1000 * uiDelay uiCfg)

enterLoop :: Cfg -> UICfg -> DeviceContext -> IO ()
enterLoop cfg uiCfg deviceContext =
  do gen <- cfgMakeGen cfg 0
     loop (ctx gen)
  where ctx gen = Ctx { cfg           = cfg
                      , uiCfg         = uiCfg
                      , stats         = mempty
                      , deviceContext = deviceContext
                      , rndGen        = gen
                      }

loop :: Ctx -> IO ()
loop ctx@(Ctx {cfg, rndGen}) =
  do game <- randomGame rndGen cfg
     loopGame ctx (newPlay game) (strategy (cfgPlayer cfg) (cfgStartMove cfg))

loopGame :: Ctx -> Play -> Strategy () -> IO ()
loopGame ctx play strategy =
  do present ctx (draw ctx play)
     loopStrategy ctx play strategy

loopStrategy :: Ctx -> Play -> Strategy () -> IO ()
loopStrategy ctx play strategy =
  do step <- runStrategy (rndGen ctx) strategy
     case step of
      Pure _                      -> surrender
      Free (PosInfo ps strategy') -> handlePosInfo ps strategy'
      Free (OpenEmpty p k)        -> handleOpenEmpty k (openEmpty play p)
      Free (MarkMine p strategy') -> handleMarkMine strategy' (markMine play p)
      Free (GetPlay k)            -> nextStep (k play)
      _                           -> error "can't happen"

  where handlePosInfo ps strategy =
          do present ctx (drawPosInfo play ps)
             nextStep strategy

        handleOpenEmpty k (play, Left err) = handleError play err (k [])
        handleOpenEmpty k (play, Right r)  = success play (k r)

        handleMarkMine strategy (play, Left err) = handleError play err strategy
        handleMarkMine strategy (play, Right ()) = success play strategy

        restart :: (PlayStats -> PlayStats) -> IO ()
        restart inc = loop (ctx {stats = inc stats})
          where Ctx {stats} = ctx

        continue play strategy = loopGame ctx play strategy
        nextStep strategy      = loopStrategy ctx play strategy

        surrender =
          do present ctx (draw ctx play >> drawError "Player surrenders")
             restart incStalled

        handleError _ ErrorNoChange strategy = nextStep strategy
        handleError play err _               =
          do present ctx (draw ctx play >> drawError (describeError err))
             restart incLost

        describeError ErrorKilled = "Player explodes on a mine"
        describeError ErrorFired  = "Player is fired due to incompetence"
        describeError _           = error "can't happen"

        success play strategy
          | isWon play =
              do present ctx (draw ctx play >> drawMsg "Player wins")
                 restart incWon
          | otherwise  = continue play strategy
