{-# LANGUAGE ApplicativeDo #-}

module CmdArgs
  ( run
  ) where

import Control.Monad (join)

import Options.Applicative
  ( Parser
  , command
  , execParser
  , fullDesc
  , helper
  , hsubparser
  , info
  , progDesc
  )

import Config (Config)
import qualified Config

import qualified Mode.Bench as ModeBench
import qualified Mode.UI as ModeUI

parseMode :: Parser (Config -> IO ())
parseMode = hsubparser (modeUI <> modeBench)
  where
    -- TODO: commands and descriptions should go inside modes as well
    modeUI = command "ui" $ info ModeUI.parse uiDesc
    uiDesc = progDesc "View a bot play using Web interface"
    modeBench = command "bench" $ info ModeBench.parse benchDesc
    benchDesc = progDesc "Benchmark bot's performance"

parse :: Parser (IO ())
parse = do
  cfg <- Config.parse
  run <- parseMode

  return $ run cfg

run :: IO ()
run = join $ execParser parserInfo
  where
    desc = progDesc "View and benchmark minesweeper bots."
    parserInfo = info (helper <*> parse) (desc <> fullDesc)
