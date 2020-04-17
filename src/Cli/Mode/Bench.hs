module Cli.Mode.Bench
  ( mode
  ) where

import Control.Concurrent.Async
  ( Concurrently(Concurrently)
  , replicateConcurrently
  , runConcurrently
  )
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
import System.Console.AsciiProgress
  ( Options(pgCompletedChar, pgFormat, pgOnCompletion, pgPendingChar,
        pgTotal)
  )
import qualified System.Console.AsciiProgress as Progress

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
import qualified GameRunner as GameRunner
import Player (strategy)
import Stats (Stats)
import qualified Stats
import Utils.Chan (Chan)
import qualified Utils.Chan as Chan
import Utils.Random (StdGen)
import qualified Utils.Random as Random

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

doRun :: Int -> Int -> Config -> IO Stats
doRun numIters numWorkers cfg = do
  jobs <- Chan.new
  gen <- Config.getRandomGen cfg

  withProgressBar numIters $ \tick ->
    runConcurrently $
      Concurrently (dispatcher gen numIters jobs tick) *>
      Concurrently (runWorkers numWorkers jobs cfg)

withProgressBar :: Int -> ((Int -> IO ()) -> IO a) -> IO a
withProgressBar ticks body =
  Progress.displayConsoleRegions $ do
    progressBar <- Progress.newProgressBar config
    let tick n = Progress.tickN progressBar n
    body tick <* Progress.complete progressBar

  where
    config =
      Progress.def
        { pgTotal = toInteger ticks
        , pgCompletedChar = '#'
        , pgPendingChar = '-'
        , pgFormat = ":bar :percent (:etas remaining)"
        , pgOnCompletion = Just "Completed in :elapseds"
        }

dispatcher :: StdGen -> Int -> Chan StdGen -> (Int -> IO ()) -> IO ()
dispatcher gen numIters chan tick = do
  delay <- mkDelay
  loop gens delay 0
  Chan.close chan
  where
    gens = take numIters $ Random.splits gen

    -- Update progress every 200 milliseconds.
    updateInterval = 200 * 1000
    mkDelay = Chan.newDelay updateInterval

    loop [] _ iters = tick iters
    loop gs@(g:rest) delay iters = do
      ok <- Chan.put chan g delay
      if ok
        then loop rest delay (iters + 1)
        else do
          tick iters
          delay' <- mkDelay
          loop gs delay' 0

worker :: Chan StdGen -> Config -> IO Stats
worker jobs cfg = loop mempty
  where
    loop !stats =
      Chan.take jobs >>= \case
        Nothing -> return stats
        Just gen -> workerIter gen cfg stats >>= loop

workerIter :: StdGen -> Config -> Stats -> IO Stats
workerIter gen cfg stats = do
  let (gameGen, runnerGen) = Random.split gen
  let game = randomGame gameGen cfg

  result <- GameRunner.run runnerGen game (strategy player startMove)
  return $ Stats.update result stats

  where
    startMove = Config.startMove cfg
    player = Config.player cfg

runWorkers :: Int -> Chan StdGen -> Config -> IO Stats
runWorkers numWorkers jobs cfg =
  mconcat <$> replicateConcurrently numWorkers (worker jobs cfg)
