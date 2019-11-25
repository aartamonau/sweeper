{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}

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

import Play (PlayError(ErrorNoChange), isWon, markMine, newPlay, openEmpty)
import Player
  ( FreeF(Free, Pure)
  , Move(GetPlay, MarkMine, OpenEmpty, PosInfo)
  , runStrategy
  , strategy
  )
import PlayStats (PlayStats, incLost, incStalled, incWon)
import Rand (Gen)

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

  let jobs = [(i, workerIters numIters numWorkers' i) | i <- [0..numWorkers'-1]]
  mapConcurrently (uncurry $ worker cfg) jobs >>= print . mconcat

workerIters :: Int -> Int -> Int -> Int
workerIters total workers i = total `div` workers + extra
  where
    rem = total `mod` workers
    extra
      | i < rem   = 1
      | otherwise = 0

worker :: Config -> Int -> Int -> IO PlayStats
worker cfg tid n = do
  gen <- Config.makeGen cfg tid
  loop n gen mempty
  where
    loop 0 _   stats = return stats
    loop i gen stats = iter cfg gen stats >>= loop (i - 1) gen

iter :: Config -> Gen -> PlayStats -> IO PlayStats
iter cfg gen stats = do
  game <- randomGame gen cfg

  let initStrategy = strategy (Config.player cfg) (Config.startMove cfg)
  loop (newPlay game) initStrategy

  where
    loop play s
      | isWon play = return $ incWon stats
      | otherwise  = runStrategy gen s >>= handleStep play

    handleStep _    (Pure _)    = return $ incStalled stats
    handleStep play (Free move) = handleMove play move

    handleMove play (PosInfo _ s)   = loop play s
    handleMove play (OpenEmpty p k) = handleOpenEmpty play p k
    handleMove play (MarkMine p s)  = handleMarkMine play p s
    handleMove play (GetPlay k)     = loop play (k play)
    handleMove _ _                  = error "can't happen"

    handleOpenEmpty play p k =
      case openEmpty play p of
        (play', Right ps) -> loop play' (k ps)
        (_, Left err)     -> handleError play (k []) err

    handleMarkMine play p s =
      case markMine play p of
        (play', Right ()) -> loop play' s
        (_, Left err)     -> handleError play s err

    handleError play s ErrorNoChange = loop play s
    handleError _ _ _                = return $ incLost stats
