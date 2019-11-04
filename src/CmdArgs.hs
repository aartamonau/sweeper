module CmdArgs
  ( Mode(ModeUI, ModeBench)
  , UICfg
  , BenchCfg
  , uiInteractive
  , uiDelay
  , benchNumIters
  , benchNumWorkers
  , run
  ) where

import GHC.Conc (getNumProcessors)

import Options.Applicative
  ( Parser
  , command
  , execParser
  , flag
  , fullDesc
  , help
  , helper
  , hsubparser
  , info
  , long
  , metavar
  , option
  , progDesc
  , showDefault
  , value
  )

import qualified CmdArgs.Read as Read

import Config (Config)
import qualified Config

data UICfg =
  UICfg
    { uiInteractive :: Bool
    , uiDelay :: Int
    }

data BenchCfg =
  BenchCfg
    { benchNumIters :: Int
    , benchNumWorkers :: Int
    }

data Mode = ModeUI UICfg | ModeBench BenchCfg

data SystemEnv =
  SystemEnv
    { numCPUs :: Int
    }

parseModeUI :: Parser UICfg
parseModeUI =
  UICfg
    <$> flag True False (long "non-interactive"
                         <> help "Run in non-interactive mode")
    <*> option Read.positiveInt
          (long "delay"
           <> metavar "DELAY"
           <> value 200
           <> showDefault
           <> help "Delay (in ms) to use in non-interactive mode")

parseModeBench :: SystemEnv -> Parser BenchCfg
parseModeBench (SystemEnv {numCPUs}) =
  BenchCfg
    <$> option Read.positiveInt
          (long "num-iters"
           <> metavar "ITERS"
           <> value 1000
           <> showDefault
           <> help "Number of games to benchmark the bot on")
    <*> option Read.positiveInt
          (long "num-workers"
           <> metavar "WORKERS"
           <> value numCPUs
           <> showDefault
           <> help "Number of workers to run benchmark on")

parseMode :: SystemEnv -> Parser Mode
parseMode env = hsubparser (modeUI <> modeBench)
  where
    modeUI = command "ui" $ info (ModeUI <$> parseModeUI) uiDesc
    uiDesc = progDesc "View a bot play using Web interface"
    modeBench =
      command "bench" $ info (ModeBench <$> parseModeBench env) benchDesc
    benchDesc = progDesc "Benchmark bot's performance"

parse :: SystemEnv -> Parser (Config, Mode)
parse env = (,) <$> Config.parse <*> parseMode env

getSystemEnv :: IO (SystemEnv)
getSystemEnv = SystemEnv <$> getNumProcessors

run :: (Config -> Mode -> IO ()) -> IO ()
run body = do
  env <- getSystemEnv
  let parserInfo = info (helper <*> parse env) (desc <> fullDesc)
  execParser parserInfo >>= uncurry body

  where
    desc = progDesc "View and benchmark minesweeper bots."
