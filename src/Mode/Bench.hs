module Mode.Bench
       (
         run
       ) where

import CmdArgs (Cfg, BenchCfg,
                benchNumIters)

run :: Cfg -> BenchCfg -> IO ()
run _ benchCfg =
  putStrLn $ "Number of iterations: " ++ show (benchNumIters benchCfg)
