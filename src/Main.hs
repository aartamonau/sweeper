{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent (threadDelay)

import Game
import Play
import Player

import CmdArgs
import UI

import qualified Player.Dummy as Dummy
import qualified Player.SinglePoint as SinglePoint

main :: IO ()
main = runWithCfg $ \cfg -> runUI (enterLoop cfg)

withWinMsg :: String -> UI -> UI
withWinMsg msg ui = ui { msg = Just (Win msg) }

withErrorMsg :: Maybe Pos -> String -> UI -> UI
withErrorMsg p msg ui = ui { msg = Just (Error p msg) }

withPosInfo :: [(Pos, String)] -> UI -> UI
withPosInfo ps ui = ui { posInfo = ps }

withPlay :: Play -> UI -> UI
withPlay play ui = ui { play = play }

redraw :: (?cfg :: Cfg) => DeviceContext -> UI -> IO ()
redraw context ui =
  do drawUI context ui
     if interactive ?cfg
       then waitKeypress context
       else threadDelay (1000 * delay ?cfg)

enterLoop :: Cfg -> DeviceContext -> IO ()
enterLoop cfg = let ?cfg = cfg in loop

loop :: (?cfg :: Cfg) => DeviceContext -> IO ()
loop context =
  do let (rows, cols, mines) = fieldSpec ?cfg
     let start = (rows `div` 2, cols `div` 2)

     game <- randomGame rows cols mines start

     let play   = newPlay game
     -- let player = Dummy.newPlayer start
     let player = SinglePoint.newPlayer start

     let Player {..} = player

     let ui = UI { play    = play
                 , game    = game
                 , msg     = Nothing
                 , posInfo = []
                 }

     loopGame ui strategy context

loopGame :: (?cfg :: Cfg) => UI -> Strategy () -> DeviceContext -> IO ()
loopGame ui strategy context =
  do redraw context ui
     loopStrategy ui strategy context

loopStrategy :: (?cfg :: Cfg) => UI -> Strategy () -> DeviceContext -> IO ()
loopStrategy ui@(UI {..}) strategy context =
  do step <- runFreeT strategy
     case step of
      Pure _                      -> surrender
      Free (PosInfo ps strategy') -> handlePosInfo ps strategy'
      Free (OpenEmpty p k)        -> handleOpenEmpty p k (openEmpty play p)
      Free (MarkMine p strategy') -> handleMarkMine p strategy' (markMine play p)
      Free (GetPlay k)            -> nextStep (k play)

  where handlePosInfo ps strategy =
          do redraw context (withPosInfo ps ui)
             nextStep strategy

        handleOpenEmpty p k (Left err)        = handleError p err (k [])
        handleOpenEmpty _ k (Right (r, play)) = success play (k r)

        handleMarkMine p strategy (Left err)   = handleError p err strategy
        handleMarkMine _ strategy (Right play) = success play strategy

        restart                = loop context
        continue play strategy = loopGame (withPlay play ui) strategy context
        nextStep strategy      = loopStrategy ui strategy context

        surrender =
          do redraw context (withErrorMsg Nothing "Player surrenders" ui)
             restart

        handleError _ ErrorNoChange strategy = nextStep strategy
        handleError p err _                  =
          do redraw context (withErrorMsg (Just p) (describeError err) ui)
             restart

        describeError ErrorKilled = "Player explodes on a mine"
        describeError ErrorFired  = "Player is fired due to incompetence"
        describeError _           = error "can't happen"

        success play strategy
          | isFinished play =
              do redraw context (withWinMsg "Player wins" (withPlay play ui))
                 restart
          | otherwise       = continue play strategy
