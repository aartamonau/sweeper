{-# LANGUAGE ApplicativeDo #-}

module Cli.Mode.UI
  ( mode
  ) where

import Control.Concurrent (threadDelay)
import Options.Applicative
  ( Parser
  , flag
  , help
  , long
  , metavar
  , option
  , showDefault
  , value
  )

import Cli.Config (Config)
import qualified Cli.Config as Config
import Cli.Mode.Common (randomPlay)
import Cli.Mode.Type (Mode(Mode))
import qualified Cli.Mode.Type
import qualified Cli.Read as Read

import Play (Play, PlayError(ErrorFired, ErrorKilled, ErrorNoChange))
import qualified Play as Play
import Player
  ( FreeF(Free, Pure)
  , Move(GetPlay, MarkMine, OpenEmpty, PosInfo)
  , Player(name, strategy)
  , Strategy
  , runStrategy
  )
import PlayStats (PlayStats, incLost, incStalled, incWon)
import Rand (Gen)

import UI.UI
  ( DeviceContext
  , Draw
  , UI(UI, play, playerName, stats)
  , display
  , drawError
  , drawMsg
  , drawPosInfo
  , drawUI
  , runUI
  , waitKeypress
  )

data UICfg =
  UICfg
    { delay :: Int
    , interactive :: Bool
    }

mode :: Mode
mode = Mode {name, help, parse}
  where
    name = "ui"
    help = "View a bot play using Web interface"

parse :: Parser (Config -> IO ())
parse = do
  delay <- option Read.positiveInt
             (long "delay"
              <> metavar "DELAY"
              <> value 200
              <> showDefault
              <> help "Delay (in ms) to use in non-interactive mode")
  interactive <- flag True False
                   (long "non-interactive"
                    <> help "Run in non-interactive mode")

  return $ run (UICfg {delay, interactive})

data Ctx =
  Ctx
    { cfg           :: Config
    , uiCfg         :: UICfg
    , stats         :: PlayStats
    , deviceContext :: DeviceContext
    , rndGen        :: Gen
    }

run :: UICfg -> Config -> IO ()
run uiCfg  = runUI . enterLoop uiCfg

draw :: Ctx -> Play -> Draw ()
draw (Ctx {stats, cfg}) play = drawUI $ UI {play, stats, playerName}
  where
    playerName = name $ Config.player cfg

present :: Ctx -> Draw () -> IO ()
present ctx@(Ctx {deviceContext}) d = display deviceContext d >> wait ctx

wait :: Ctx -> IO ()
wait (Ctx {deviceContext, uiCfg})
  | interactive uiCfg = waitKeypress deviceContext
  | otherwise         = threadDelay (1000 * delay uiCfg)

enterLoop :: UICfg -> Config -> DeviceContext -> IO ()
enterLoop uiCfg cfg deviceContext = do
  gen <- Config.makeGen cfg 0
  loop (ctx gen)
  where
    ctx gen =
      Ctx
        { cfg = cfg
        , uiCfg = uiCfg
        , stats = mempty
        , deviceContext = deviceContext
        , rndGen = gen
        }

loop :: Ctx -> IO ()
loop ctx@(Ctx {cfg, rndGen}) = do
  play <- randomPlay rndGen cfg
  loopGame ctx play $
    strategy (Config.player cfg) (Config.startMove cfg)

loopGame :: Ctx -> Play -> Strategy () -> IO ()
loopGame ctx play strategy = do
  present ctx (draw ctx play)
  loopStrategy ctx play strategy

loopStrategy :: Ctx -> Play -> Strategy () -> IO ()
loopStrategy ctx play strategy = do
  step <- runStrategy (rndGen ctx) strategy
  case step of
    Pure _                      -> surrender
    Free (PosInfo ps strategy') -> handlePosInfo ps strategy'
    Free (OpenEmpty p k)        -> handleOpenEmpty k (Play.openEmpty play p)
    Free (MarkMine p strategy') ->
      handleMarkMine strategy' (Play.markMine play p)
    Free (GetPlay k)            -> nextStep (k play)
    _                           -> error "can't happen"

  where
    handlePosInfo ps strategy = do
      present ctx (drawPosInfo play ps)
      nextStep strategy

    handleOpenEmpty k (play, Left err) = handleError play err (k [])
    handleOpenEmpty k (play, Right r)  = success play (k r)

    handleMarkMine strategy (play, Left err) = handleError play err strategy
    handleMarkMine strategy (play, Right ()) = success play strategy

    restart :: (PlayStats -> PlayStats) -> IO ()
    restart inc = loop (ctx {stats = inc stats})
      where
        Ctx {stats} = ctx

    continue play strategy = loopGame ctx play strategy
    nextStep strategy      = loopStrategy ctx play strategy

    surrender = do
      present ctx (draw ctx play >> drawError "Player surrenders")
      restart incStalled

    handleError _ ErrorNoChange strategy = nextStep strategy
    handleError play err _               = do
      present ctx (draw ctx play >> drawError (describeError err))
      restart incLost

    describeError ErrorKilled = "Player explodes on a mine"
    describeError ErrorFired  = "Player is fired due to incompetence"
    describeError _           = error "can't happen"

    success play strategy
      | Play.isWon play = do
        present ctx (draw ctx play >> drawMsg "Player wins")
        restart incWon
      | otherwise = continue play strategy
