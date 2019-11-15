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
  , showDefaultWith
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
    , benchNumWorkers :: Maybe Int
    }

data Mode = ModeUI UICfg | ModeBench BenchCfg

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

parseModeBench :: Parser BenchCfg
parseModeBench =
  BenchCfg
    <$> option Read.positiveInt
          (long "num-iters"
           <> metavar "ITERS"
           <> value 1000
           <> showDefault
           <> help "Number of games to benchmark the bot on")
    <*> option (Just <$> Read.positiveInt)
          (long "num-workers"
           <> metavar "WORKERS"
           <> value Nothing
           <> showDefaultWith (const "number of logical CPUs")
           <> help "Number of workers to run benchmark on")

parseMode :: Parser Mode
parseMode = hsubparser (modeUI <> modeBench)
  where
    modeUI = command "ui" $ info (ModeUI <$> parseModeUI) uiDesc
    uiDesc = progDesc "View a bot play using Web interface"
    modeBench = command "bench" $ info (ModeBench <$> parseModeBench) benchDesc
    benchDesc = progDesc "Benchmark bot's performance"

parse :: Parser (Config, Mode)
parse = (,) <$> Config.parse <*> parseMode

run :: (Config -> Mode -> IO ()) -> IO ()
run body = execParser parserInfo >>= uncurry body
  where
    desc = progDesc "View and benchmark minesweeper bots."
    parserInfo = info (helper <*> parse) (desc <> fullDesc)
