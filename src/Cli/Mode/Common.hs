module Cli.Mode.Common
  ( randomPlay
  ) where

import Cli.Config (Config)
import qualified Cli.Config as Config

import Play (Play)
import qualified Play as Play

import Rand (Gen, runRand)

randomPlay :: Gen -> Config -> IO Play
randomPlay gen cfg = runRand (Play.random rows cols mines startMove buffer) gen
  where
    (rows, cols, mines) = Config.fieldSpec cfg
    startMove           = Config.startMove cfg
    buffer              = Config.buffer cfg
