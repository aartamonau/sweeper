module Cli.Mode.Bench
  ( mode
  ) where

import Control.Concurrent.Async (concurrently, replicateConcurrently)
import Data.List (unfoldr)
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
import System.Random (StdGen, split)

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
  gen <- Config.getRandomGen cfg
  snd <$>
    concurrently (dispatcher gen numIters jobs) (runWorkers numWorkers jobs cfg)

dispatcher :: StdGen -> Int -> Chan StdGen -> IO ()
dispatcher gen numIters chan =
  mapM_ (Chan.put chan) (take numIters gens) >> Chan.close chan
  where
    gens = unfoldr (Just . split) gen

worker :: Chan StdGen -> Config -> IO PlayStats
worker jobs cfg = loop mempty
  where
    loop !stats =
      Chan.take jobs >>= \case
        Nothing -> return stats
        Just gen -> workerIter gen cfg stats >>= loop

workerIter :: StdGen -> Config -> PlayStats -> IO PlayStats
workerIter gen cfg stats = do
  let (gameGen, runnerGen) = split gen
  let game = randomGame gameGen cfg

  GameRunner.run runnerGen game (strategy player startMove) >>= \case
    GameLost -> return (incLost stats)
    GameWon -> return (incWon stats)

  where
    startMove = Config.startMove cfg
    player = Config.player cfg

runWorkers :: Int -> Chan StdGen -> Config -> IO PlayStats
runWorkers numWorkers jobs cfg =
  mconcat <$> replicateConcurrently numWorkers (worker jobs cfg)
