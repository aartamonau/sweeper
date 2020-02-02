module Cli.Mode.UI
  ( mode
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (foldM_)
import Options.Applicative
  ( Parser
  , help
  , long
  , metavar
  , option
  , showDefault
  , switch
  , value
  )

import Cli.Config (Config)
import qualified Cli.Config as Config
import Cli.Mode.Common (randomGame)
import Cli.Mode.Type (Mode(Mode))
-- Cli.Mode.Type is imported qualified only for Mode's record fields, which,
-- somewhat confusingly, can be used unqualified in conjunction with
-- -XDisambiguateRecordFields.
--
-- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-DisambiguateRecordFields
import qualified Cli.Mode.Type as Type (Mode(name, help, parse))
import qualified Cli.Read as Read
import Game (Game)
import GameRunner (GameResult(GameLost, GameWon))
import qualified GameRunner as GameRunner
import Player (Player(name, strategy))
import Stats (Stats, incLost, incWon)
import UI.UI
  ( DeviceContext
  , Draw
  , UI(UI, game, playerName, stats)
  , display
  , drawError
  , drawMsg
  , drawUI
  , runUI
  , waitKeypress
  )
import Utils.Random (StdGen)
import qualified Utils.Random as Random

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
  interactive <- not <$> switch (long "non-interactive"
                                 <> help "Run in non-interactive mode")

  return $ run (UICfg {delay, interactive})

data Ctx =
  Ctx
    { cfg           :: Config
    , uiCfg         :: UICfg
    , stats         :: Stats
    , deviceContext :: DeviceContext
    }

run :: UICfg -> Config -> IO ()
run uiCfg = runUI . loop uiCfg

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

loop :: UICfg -> Config -> DeviceContext -> IO ()
loop uiCfg cfg deviceContext = do
  gens <- Random.splits <$> Config.getRandomGen cfg
  foldM_ iter ctx gens
  where
    ctx =
      Ctx
        { cfg = cfg
        , uiCfg = uiCfg
        , stats = mempty
        , deviceContext = deviceContext
        }

iter :: Ctx -> StdGen -> IO Ctx
iter ctx@(Ctx {cfg, stats}) gen = do
  presentGame game
  result <-
    GameRunner.trace presentGame runnerGen game (strategy player startMove)
  presentResult result
  return (ctx {stats = incStats result} :: Ctx)

  where
    (gameGen, runnerGen) = Random.split gen
    game = randomGame gameGen cfg

    startMove = Config.startMove cfg
    player = Config.player cfg
    presentGame game = present ctx (draw ctx game)

    showMsg msg = present ctx (drawMsg msg)
    showError msg = present ctx (drawError msg)

    presentResult GameWon = showMsg "Player wins"
    presentResult GameLost = showError "Player loses"

    incStats GameWon = incWon stats
    incStats GameLost = incLost stats
