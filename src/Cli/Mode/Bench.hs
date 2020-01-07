module Cli.Mode.Bench
  ( mode
  ) where

import Control.Concurrent.Async (concurrently, replicateConcurrently)
import Control.Monad (replicateM_)
import Data.Maybe (fromMaybe)
import GHC.Conc (getNumProcessors)
import Options.Applicative
  ( Parser
  , help
  , long
  , metavar
  , option
  , showDefault
  , showDefaultWith
  , value
  )

import Cli.Config (Config)
import qualified Cli.Config as Config
import Cli.Mode.Common (randomGame)
import Cli.Mode.Type (Mode(Mode))
import qualified Cli.Mode.Type
import qualified Cli.Read as Read
import GameRunner (GameResult(GameLost, GameWon))
import qualified GameRunner as GameRunner
import Player (strategy)
import PlayStats (PlayStats, incLost, incWon)
import Utils.Chan (Chan)
import qualified Utils.Chan as Chan

data BenchCfg =
  BenchCfg
    { numIters :: Int
    , numWorkers :: Maybe Int
    }

mode :: Mode
mode = Mode {name, help, parse}
  where
    name = "bench"
    help = "Benchmark bot's performance"

parse :: Parser (Config -> IO ())
parse = do
  numIters <- option Read.positiveInt
                (long "num-iters"
                 <> metavar "ITERS"
                 <> value 1000
                 <> showDefault
                 <> help "Number of games to benchmark the bot on")
  numWorkers <- option (Just <$> Read.positiveInt)
                  (long "num-workers"
                   <> metavar "WORKERS"
                   <> value Nothing
                   <> showDefaultWith (const "number of logical CPUs")
                   <> help "Number of workers to run benchmark on")

  return $ run (BenchCfg {numWorkers, numIters})

run :: BenchCfg -> Config -> IO ()
run (BenchCfg {numWorkers, numIters}) cfg = do
  numCPUs <- getNumProcessors
  let numWorkers' = fromMaybe numCPUs numWorkers

  putStrLn $ "Number of iterations: " ++ show numIters
  putStrLn $ "Number of workers: " ++ show numWorkers'

  doRun numIters numWorkers' cfg >>= print

doRun :: Int -> Int -> Config -> IO PlayStats
doRun numIters numWorkers cfg = do
  jobs <- Chan.new
  snd <$>
    concurrently (dispatcher numIters jobs) (runWorkers numWorkers jobs cfg)

dispatcher :: Int -> Chan () -> IO ()
dispatcher numIters chan =
  replicateM_ numIters (Chan.put chan ()) >> Chan.close chan

worker :: Chan () -> Config -> IO PlayStats
worker jobs cfg = loop mempty
  where
    loop !stats =
      Chan.take jobs >>= \case
        Nothing -> return stats
        Just _ -> workerIter cfg stats >>= loop

workerIter :: Config -> PlayStats -> IO PlayStats
workerIter cfg stats = do
  game <- randomGame cfg

  GameRunner.run game (strategy player startMove) >>= \case
    GameLost -> return (incLost stats)
    GameWon -> return (incWon stats)

  where
    startMove = Config.startMove cfg
    player = Config.player cfg

runWorkers :: Int -> Chan () -> Config -> IO PlayStats
runWorkers numWorkers jobs cfg =
  mconcat <$> replicateConcurrently numWorkers (worker jobs cfg)
