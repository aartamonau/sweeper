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
import Cli.Mode.Common (randomGame)
import Cli.Mode.Type (Mode(Mode))
import qualified Cli.Mode.Type
import qualified Cli.Read as Read

import Game (Game, PlayError(ErrorKilled, ErrorAlreadyPlayed))
import qualified Game as Game
import Player
  ( FreeF(Free, Pure)
  , Move(GetGame, MarkMine, OpenEmpty, PosInfo)
  , Player(name, strategy)
  , Strategy
  , runStrategy
  )
import PlayStats (PlayStats, incLost, incStalled, incWon)

import UI.UI
  ( DeviceContext
  , Draw
  , UI(UI, game, playerName, stats)
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
    }

run :: UICfg -> Config -> IO ()
run uiCfg  = runUI . enterLoop uiCfg

draw :: Ctx -> Game -> Draw ()
draw (Ctx {stats, cfg}) game = drawUI $ UI {game, stats, playerName}
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
  loop ctx
  where
    ctx =
      Ctx
        { cfg = cfg
        , uiCfg = uiCfg
        , stats = mempty
        , deviceContext = deviceContext
        }

loop :: Ctx -> IO ()
loop ctx@(Ctx {cfg}) = do
  game <- randomGame cfg
  loopGame ctx game $
    strategy (Config.player cfg) (Config.startMove cfg)

loopGame :: Ctx -> Game -> Strategy () -> IO ()
loopGame ctx game strategy = do
  present ctx (draw ctx game)
  loopStrategy ctx game strategy

loopStrategy :: Ctx -> Game -> Strategy () -> IO ()
loopStrategy ctx game strategy = do
  step <- runStrategy strategy
  case step of
    Pure _                      -> surrender
    Free (PosInfo ps strategy') -> handlePosInfo ps strategy'
    Free (OpenEmpty p k)        -> handleOpenEmpty k (Game.openEmpty game p)
    Free (MarkMine p strategy') ->
      handleMarkMine strategy' (Game.markMine game p)
    Free (GetGame k)            -> nextStep (k game)
    _                           -> error "can't happen"

  where
    handlePosInfo ps strategy = do
      present ctx (drawPosInfo game ps)
      nextStep strategy

    handleOpenEmpty k (game, Left err) = handleError game err (k [])
    handleOpenEmpty k (game, Right r)  = success game (k r)

    handleMarkMine strategy (game, Left err) = handleError game err strategy
    handleMarkMine strategy (game, Right ()) = success game strategy

    restart :: (PlayStats -> PlayStats) -> IO ()
    restart inc = loop (ctx {stats = inc stats})
      where
        Ctx {stats} = ctx

    continue game strategy = loopGame ctx game strategy
    nextStep strategy      = loopStrategy ctx game strategy

    surrender = do
      present ctx (draw ctx game >> drawError "Player surrenders")
      restart incStalled

    handleError game err _               = do
      present ctx (draw ctx game >> drawError (describeError err))
      restart incLost

    describeError ErrorKilled         = "Player explodes on a mine"
    describeError ErrorAlreadyPlayed  =
      "Player attempted to play a square that had already been opened"

    success game strategy
      | Game.isWon game = do
        present ctx (draw ctx game >> drawMsg "Player wins")
        restart incWon
      | otherwise = continue game strategy
