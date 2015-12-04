{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Game
import Play
import Player

import UI

import qualified Player.Dummy as Dummy
import qualified Player.Cheater as Cheater
import qualified Player.SinglePoint as SinglePoint

main :: IO ()
main = runUI loop

withWinMsg :: String -> UI -> UI
withWinMsg msg ui = ui { msg = Just (Win msg) }

withErrorMsg :: Maybe Pos -> String -> UI -> UI
withErrorMsg p msg ui = ui { msg = Just (Error p msg) }

withPosInfo :: [(Pos, String)] -> UI -> UI
withPosInfo ps ui = ui { posInfo = ps }

withPlay :: Play -> UI -> UI
withPlay play ui = ui { play = play }

loop :: DeviceContext -> IO ()
loop context =
  do let cols  = 10
     let rows  = 10
     let mines = 10
     let start = (rows `div` 2, cols `div` 2)

     game <- randomGame rows cols mines start

     let play   = newPlay game
     -- let player = Cheater.newPlayer game start
     -- let player = Dummy.newPlayer start
     let player = SinglePoint.newPlayer start

     let Player {..} = player

     let ui = UI { play    = play
                 , game    = game
                 , msg     = Nothing
                 , posInfo = []
                 }

     loopGame ui strategy context

loopGame :: UI -> Strategy () -> DeviceContext -> IO ()
loopGame ui strategy context =
  do drawUI context ui
     loopStrategy ui strategy context

loopStrategy :: UI -> Strategy () -> DeviceContext -> IO ()
loopStrategy ui@(UI {..}) strategy context =
  do step <- runFreeT strategy
     case step of
      Pure _                      -> surrender
      Free (PosInfo ps strategy') -> handlePosInfo ps strategy'
      Free (OpenEmpty p k)        -> handleOpenEmpty p k (openEmpty play p)
      Free (MarkMine p strategy') -> handleMarkMine p strategy' (markMine play p)
      Free (GetPlay k)            -> nextStep (k play)

  where handlePosInfo ps strategy =
          do drawUI context (withPosInfo ps ui)
             nextStep strategy

        handleOpenEmpty p k (Left err)        = handleError p err (k [])
        handleOpenEmpty _ k (Right (r, play)) = success play (k r)

        handleMarkMine p strategy (Left err)   = handleError p err strategy
        handleMarkMine _ strategy (Right play) = success play strategy

        restart                = loop context
        continue play strategy = loopGame (withPlay play ui) strategy context
        nextStep strategy      = loopStrategy ui strategy context

        surrender =
          do drawUI context (withErrorMsg Nothing "Player surrenders" ui)
             restart

        handleError _ ErrorNoChange strategy = nextStep strategy
        handleError p err _                  =
          do drawUI context (withErrorMsg (Just p) (describeError err) ui)
             restart

        describeError ErrorKilled        = "Player explodes on a mine"
        describeError ErrorFired         = "Player is fired due to incompetence"
        describeError _                  = error "can't happen"

        success play strategy
          | isFinished play =
              do drawUI context (withWinMsg "Player wins" (withPlay play ui))
                 restart
          | otherwise       = continue play strategy
