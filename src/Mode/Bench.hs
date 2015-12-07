module Mode.Bench
       (
         run
       ) where

import Data.IORef (IORef, newIORef, atomicModifyIORef')

import CmdArgs (Cfg, BenchCfg,
                cfgPlayer, cfgStartMove, benchNumIters)

import Play (PlayError(ErrorNoChange),
             newPlay, isWon, markMine, openEmpty)
import Player (Move(OpenEmpty, MarkMine, GetPlay, PosInfo),
               FreeF(Free, Pure),
               strategy, runFreeT)
import PlayStats (PlayStats, incWon, incLost, incStalled)

import Mode.Common (randomGame)

run :: Cfg -> BenchCfg -> IO ()
run cfg benchCfg =
  do putStrLn $ "Number of iterations: " ++ show iters

     workRef <- newIORef iters
     stats <- worker cfg workRef

     print stats

  where iters  = benchNumIters benchCfg

getWork :: IORef Int -> IO Bool
getWork ref = atomicModifyIORef' ref modify
  where modify v | v > 0     = (v-1, True)
                 | v == 0    = (0, False)
                 | otherwise = error "impossible: negative work count"

worker :: Cfg -> IORef Int -> IO PlayStats
worker cfg ref = loop mempty
  where loop stats =
          do gotWork <- getWork ref
             if gotWork
               then iter cfg stats >>= loop
               else return stats

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
