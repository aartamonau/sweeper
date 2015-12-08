{-# LANGUAGE BangPatterns #-}

module Mode.Bench
       (
         run
       ) where

import Control.Concurrent (setNumCapabilities)
import Control.Concurrent.Async (mapConcurrently)

import CmdArgs (Cfg, BenchCfg,
                cfgPlayer, cfgStartMove, benchNumIters, benchNumWorkers)

import Play (PlayError(ErrorNoChange),
             newPlay, isWon, markMine, openEmpty)
import Player (Move(OpenEmpty, MarkMine, GetPlay, PosInfo),
               FreeF(Free, Pure),
               strategy, runFreeT)
import PlayStats (PlayStats, incWon, incLost, incStalled)

import Mode.Common (randomGame)

run :: Cfg -> BenchCfg -> IO ()
run cfg benchCfg =
  do putStrLn $ "Number of iterations: " ++ show numIters
     putStrLn $ "Number of workers: " ++ show numWorkers

     setNumCapabilities numWorkers
     mapConcurrently (worker cfg) works >>= print . mconcat

  where numIters   = benchNumIters benchCfg
        numWorkers = benchNumWorkers benchCfg

        works = map (workerIters numIters numWorkers) [0..numWorkers-1]

workerIters :: Int -> Int -> Int -> Int
workerIters total workers i = total `div` workers + extra
  where rem = total `mod` workers
        extra | i < rem   = 1
              | otherwise = 0

worker :: Cfg -> Int -> IO PlayStats
worker cfg n = loop n mempty
  where loop 0 !stats = return stats
        loop i !stats = iter cfg stats >>= loop (i-1)

iter :: Cfg -> PlayStats -> IO PlayStats
iter cfg stats =
  do game <- randomGame cfg

     let initStrategy = strategy (cfgPlayer cfg) (cfgStartMove cfg)
     loop (newPlay game) initStrategy

  where loop play s | isWon play = return $ incWon stats
                    | otherwise  = runFreeT s >>= handleStep play

        handleStep _    (Pure _)    = return $ incStalled stats
        handleStep play (Free move) = handleMove play move

        handleMove play (PosInfo _ s)   = loop play s
        handleMove play (OpenEmpty p k) = handleOpenEmpty play p k
        handleMove play (MarkMine p s)  = handleMarkMine play p s
        handleMove play (GetPlay k)     = loop play (k play)

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
