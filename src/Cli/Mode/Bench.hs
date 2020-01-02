module Cli.Mode.Bench
  ( mode
  ) where

import Control.Concurrent (setNumCapabilities)
import Control.Concurrent.Async (mapConcurrently)
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
import Cli.Mode.Type (Mode(Mode))
import qualified Cli.Mode.Type
import qualified Cli.Read as Read

import GameRunner (GameResult(GameLost, GameWon))
import qualified GameRunner as GameRunner
import Player (strategy)
import PlayStats (PlayStats, incLost, incWon)

import Cli.Mode.Common (randomGame)

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

  setNumCapabilities numWorkers'

  let jobs = [workerIters numIters numWorkers' i | i <- [0..numWorkers'-1]]
  mapConcurrently (worker cfg) jobs >>= print . mconcat

workerIters :: Int -> Int -> Int -> Int
workerIters total workers i = total `div` workers + extra
  where
    rem = total `mod` workers
    extra
      | i < rem   = 1
      | otherwise = 0

worker :: Config -> Int -> IO PlayStats
worker cfg n = do
  loop n mempty
  where
    loop 0 stats = return stats
    loop i stats = iter cfg stats >>= loop (i - 1)

iter :: Config -> PlayStats -> IO PlayStats
iter cfg stats = do
  game <- randomGame cfg

  GameRunner.run game (strategy player startMove) >>= \case
    GameLost -> return (incLost stats)
    GameWon -> return (incWon stats)

  where
    startMove = Config.startMove cfg
    player = Config.player cfg
