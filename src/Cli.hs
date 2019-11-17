{-# LANGUAGE ApplicativeDo #-}

module Cli
  ( run
  ) where

import Control.Monad (join)

import Options.Applicative
  ( Parser
  , execParser
  , fullDesc
  , helper
  , info
  , progDesc
  )

import qualified Cli.Config as Config
import qualified Cli.Mode as Mode

parse :: Parser (IO ())
parse = do
  cfg <- Config.parse
  run <- Mode.parse

  return $ run cfg

run :: IO ()
run = join $ execParser parserInfo
  where
    desc = progDesc "View and benchmark minesweeper bots."
    parserInfo = info (helper <*> parse) (desc <> fullDesc)
