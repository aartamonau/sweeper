module Cli.Config
  ( Config,
    fieldSpec,
    startMove,
    buffer,
    player,
    seed,
    getRandomGen,
    parse,
  )
where

import Cli.Helpers (presentList)
import qualified Cli.Read as Read
import Control.Applicative ((<|>))
import Data.List.Extra (splitOn)
import Options.Applicative
  ( Parser,
    ReadM,
    help,
    long,
    maybeReader,
    metavar,
    option,
    showDefault,
    value,
  )
import Player (Player (name))
import qualified Player.Backtracking as Backtracking
import qualified Player.CSP as CSP
import qualified Player.Dummy as Dummy
import qualified Player.SinglePoint as SinglePoint
import Text.Read (readMaybe)
import Utils.Random (StdGen)
import qualified Utils.Random as Random

data FieldSpec
  = Easy
  | Medium
  | Hard
  | Custom Int Int Int

instance Show FieldSpec where
  show Easy = "easy"
  show Medium = "medium"
  show Hard = "hard"
  show (Custom r c m) = show r ++ "x" ++ show c ++ "x" ++ show m

data StartMove
  = Center
  | Corner

instance Show StartMove where
  show Center = "center"
  show Corner = "corner"

data Config = Config
  { rawFieldSpec :: FieldSpec,
    rawStartMove :: StartMove,
    buffer :: Int,
    player :: Player,
    seed :: Int
  }

getRandomGen :: Config -> IO StdGen
getRandomGen Config {seed} = pure $ Random.mkStdGen seed

fieldSpec :: Config -> (Int, Int, Int)
fieldSpec = decode . rawFieldSpec
  where
    decode Easy = (10, 10, 10)
    decode Medium = (16, 16, 40)
    decode Hard = (16, 30, 99)
    decode (Custom r c m) = (r, c, m)

startMove :: Config -> (Int, Int)
startMove cfg = decode $ rawStartMove cfg
  where
    decode Corner = (0, 0)
    decode Center = (rows `div` 2, columns `div` 2)
      where
        (rows, columns, _) = fieldSpec cfg

knownPlayers :: [Player]
knownPlayers =
  [ Dummy.player,
    SinglePoint.player,
    Backtracking.player,
    CSP.player
  ]

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
  option
    readPlayer
    ( long "player"
        <> metavar "PLAYER"
        <> value defaultPlayer
        <> showDefault
        <> help ("Player (one of " ++ names ++ ")")
    )
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
      <> help "Number of empty cells surrounding start position"

parseSeed :: Parser (IO Int)
parseSeed =
  option (pure <$> Read.int) $
    long "seed"
      <> metavar "SEED"
      <> value Random.randomIO
      <> help "Override default random seed"

parse :: Parser (IO Config)
parse = do
  fieldSpec <- parseFieldSpec
  startMove <- parseStartMove
  bufferZone <- parseBufferZone
  player <- parsePlayer
  mkSeed <- parseSeed

  pure $ Config fieldSpec startMove bufferZone player <$> mkSeed
