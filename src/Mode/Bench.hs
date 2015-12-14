{-# LANGUAGE BangPatterns #-}

module Mode.Bench
       (
         run
       ) where

import Control.Concurrent (setNumCapabilities)
import Control.Concurrent.Async (mapConcurrently)

import CmdArgs (Cfg, BenchCfg,
                cfgPlayer, cfgStartMove, cfgMakeGen,
                benchNumIters, benchNumWorkers)

import Play (PlayError(ErrorNoChange),
             newPlay, isWon, markMine, openEmpty)
import Player (Move(OpenEmpty, MarkMine, GetPlay, PosInfo),
               FreeF(Free, Pure),
               strategy, runStrategy)
import PlayStats (PlayStats, incWon, incLost, incStalled)
import Rand (Gen)

import Mode.Common (randomGame)

run :: Cfg -> BenchCfg -> IO ()
run cfg benchCfg =
  do putStrLn $ "Number of iterations: " ++ show numIters
     putStrLn $ "Number of workers: " ++ show numWorkers

     setNumCapabilities numWorkers
     mapConcurrently (uncurry $ worker cfg) works >>= print . mconcat

  where numIters   = benchNumIters benchCfg
        numWorkers = benchNumWorkers benchCfg

        works = [(i, workerIters numIters numWorkers i) | i <- [0..numWorkers-1]]

workerIters :: Int -> Int -> Int -> Int
workerIters total workers i = total `div` workers + extra
  where rem = total `mod` workers
        extra | i < rem   = 1
              | otherwise = 0

worker :: Cfg -> Int -> Int -> IO PlayStats
worker cfg tid n =
  do gen <- cfgMakeGen cfg tid
     loop n gen mempty
  where loop 0 _   !stats = return stats
        loop i gen !stats = iter cfg gen stats >>= loop (i-1) gen

iter :: Cfg -> Gen -> PlayStats -> IO PlayStats
iter cfg gen stats =
  do game <- randomGame gen cfg

     let initStrategy = strategy (cfgPlayer cfg) (cfgStartMove cfg)
     loop (newPlay game) initStrategy

  where loop play s | isWon play = return $ incWon stats
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
