{-# LANGUAGE BangPatterns #-}

module Mode.Bench
  ( run
  ) where

import Control.Concurrent (setNumCapabilities)
import Control.Concurrent.Async (mapConcurrently)

import CmdArgs (BenchCfg, benchNumIters, benchNumWorkers)

import Config (Config)
import qualified Config

import Play (PlayError(ErrorNoChange), isWon, markMine, newPlay, openEmpty)
import Player
  ( FreeF(Free, Pure)
  , Move(GetPlay, MarkMine, OpenEmpty, PosInfo)
  , runStrategy
  , strategy
  )
import PlayStats (PlayStats, incLost, incStalled, incWon)
import Rand (Gen)

import Mode.Common (randomGame)

run :: Config -> BenchCfg -> IO ()
run cfg benchCfg = do
  putStrLn $ "Number of iterations: " ++ show numIters
  putStrLn $ "Number of workers: " ++ show numWorkers

  setNumCapabilities numWorkers
  mapConcurrently (uncurry $ worker cfg) works >>= print . mconcat

  where
    numIters   = benchNumIters benchCfg
    numWorkers = benchNumWorkers benchCfg

    works = [(i, workerIters numIters numWorkers i) | i <- [0..numWorkers-1]]

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
    loop 0 _   !stats = return stats
    loop i gen !stats = iter cfg gen stats >>= loop (i - 1) gen

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
