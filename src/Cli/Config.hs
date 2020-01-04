module Cli.Config
  ( Config
  , fieldSpec
  , startMove
  , buffer
  , player
  , seed
  , parse
  ) where

import Control.Applicative ((<|>))
import Data.List.Extra (splitOn)
import Options.Applicative
  ( Parser
  , ReadM
  , help
  , long
  , maybeReader
  , metavar
  , option
  , showDefault
  , value
  )
import Text.Read (readMaybe)

import Cli.Helpers (presentList)
import qualified Cli.Read as Read

import Player (Player(name))

import qualified Player.Dummy as Dummy
import qualified Player.SinglePoint as SinglePoint

data FieldSpec
  = Easy
  | Medium
  | Hard
  | Custom Int Int Int

instance Show FieldSpec where
  show Easy           = "easy"
  show Medium         = "medium"
  show Hard           = "hard"
  show (Custom r c m) = show r ++ "x" ++ show c ++ "x" ++ show m

data StartMove
  = Center
  | Corner

instance Show StartMove where
  show Center = "center"
  show Corner = "corner"

data Config =
  Config
    { rawFieldSpec :: FieldSpec
    , rawStartMove :: StartMove
    , buffer       :: Int
    , player       :: Player
    , seed         :: Maybe Int
    }

fieldSpec :: Config -> (Int, Int, Int)
fieldSpec = decode . rawFieldSpec
  where
    decode Easy           = (10, 10, 10)
    decode Medium         = (16, 16, 40)
    decode Hard           = (16, 30, 99)
    decode (Custom r c m) = (r, c, m)

startMove :: Config -> (Int, Int)
startMove cfg = decode $ rawStartMove cfg
  where
    decode Corner = (0, 0)
    decode Center = (rows `div` 2, columns `div` 2)
      where
        (rows, columns, _) = fieldSpec cfg

knownPlayers :: [Player]
knownPlayers = [Dummy.player, SinglePoint.player]

defaultPlayer :: Player
defaultPlayer = head knownPlayers

readCustomFieldSpec :: ReadM FieldSpec
readCustomFieldSpec = maybeReader reader
  where
    reader value
      | [rows, columns, mines] <- splitOn "x" value =
        Custom
          <$> readMaybe rows
          <*> readMaybe columns
          <*> readMaybe mines
      | otherwise = Nothing

readFieldSpec :: ReadM FieldSpec
readFieldSpec = Read.oneOf mnemonicSpecs <|> readCustomFieldSpec
  where
    mnemonicSpecs = [("easy", Easy), ("medium", Medium), ("hard", Hard)]

parseFieldSpec :: Parser FieldSpec
parseFieldSpec =
  option readFieldSpec $
    long "field"
    <> metavar "SPEC"
    <> value Easy
    <> showDefault
    <> help "Field specification (easy, medium, hard or RxCxM)"

readPlayer :: ReadM Player
readPlayer = Read.oneOf [(name p, p) | p <- knownPlayers]

parsePlayer :: Parser Player
parsePlayer =
  option readPlayer (long "player"
                     <> metavar "PLAYER"
                     <> value defaultPlayer
                     <> showDefault
                     <> help ("Player (one of " ++ names ++ ")"))

  where
    names = presentList $ map name knownPlayers

readStartMove :: ReadM StartMove
readStartMove = Read.oneOf [("center", Center), ("corner", Corner)]

parseStartMove :: Parser StartMove
parseStartMove =
  option readStartMove $
    long "start-move"
    <> metavar "START"
    <> value Center
    <> showDefault
    <> help "Start move (center or corner)"

parseBufferZone :: Parser Int
parseBufferZone =
  option Read.nonNegativeInt $
    long "buffer-zone"
    <> metavar "ROWS"
    <> value 0
    <> showDefault
    <> help "Number of empty boxes surrounding start position"

parseSeed :: Parser (Maybe Int)
parseSeed =
  option (Just <$> Read.int) $
    long "seed"
    <> metavar "SEED"
    <> value Nothing
    <> help "Override default random seed"

parse :: Parser Config
parse =
  Config
    <$> parseFieldSpec
    <*> parseStartMove
    <*> parseBufferZone
    <*> parsePlayer
    <*> parseSeed
