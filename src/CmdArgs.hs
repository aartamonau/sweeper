module CmdArgs
       (
         Cfg
       , Mode(ModeUI, ModeBench)
       , UICfg
       , BenchCfg
       , cfgPlayer
       , cfgStartMove
       , cfgFieldSpec
       , cfgMode
       , cfgBuffer
       , cfgMakeGen
       , uiInteractive
       , uiDelay
       , benchNumIters
       , benchNumWorkers
       , runWithCfg
       )
       where

import Data.Bits (xor)
import Data.List.Split (splitOn)
import Control.Applicative ((<|>))
import GHC.Conc (getNumProcessors)
import Text.Read (readMaybe)

import Options.Applicative (Parser, ReadM,
                            execParser,
                            command, hsubparser,
                            helper, info, progDesc, fullDesc,
                            long, metavar, help, value, showDefault,
                            option, flag,
                            maybeReader)

import CmdArgs.Helpers (presentList)
import qualified CmdArgs.Read as Read
import Player (Player(name))

import qualified Player.Constraints as Constraints
import qualified Player.Dummy as Dummy
import qualified Player.SinglePoint as SinglePoint

import Rand (Gen, newGen, systemGen)


knownPlayers :: [Player]
knownPlayers = [Constraints.player, Dummy.player, SinglePoint.player]

defaultPlayer :: Player
defaultPlayer = head knownPlayers

data FieldSpec = Easy | Medium | Hard | Custom Int Int Int

instance Show FieldSpec where
  show Easy           = "easy"
  show Medium         = "medium"
  show Hard           = "hard"
  show (Custom r c m) = show r ++ "x" ++ show c ++ "x" ++ show m

data StartMove = Center | Corner

instance Show StartMove where
  show Center = "center"
  show Corner = "corner"

data UICfg =
  UICfg { uiInteractive :: Bool
        , uiDelay       :: Int
        }

data BenchCfg =
  BenchCfg { benchNumIters   :: Int
           , benchNumWorkers :: Int
           }

data Mode = ModeUI UICfg | ModeBench BenchCfg

data Cfg =
  Cfg { fieldSpec :: FieldSpec
      , player    :: Player
      , start     :: StartMove
      , buffer    :: Int
      , seed      :: Maybe Int
      , mode      :: Mode
      }

data SystemEnv =
  SystemEnv { numCPUs :: Int }

cfgFieldSpec :: Cfg -> (Int, Int, Int)
cfgFieldSpec = spec . fieldSpec
  where spec Easy           = (10, 10, 10)
        spec Medium         = (16, 16, 40)
        spec Hard           = (16, 30, 99)
        spec (Custom r c m) = (r, c, m)

cfgStartMove :: Cfg -> (Int, Int)
cfgStartMove cfg = go $ start cfg
  where go Corner = (0, 0)
        go Center = (rows `div` 2, columns `div` 2)
          where (rows, columns, _) = cfgFieldSpec cfg

cfgPlayer :: Cfg -> Player
cfgPlayer = player

cfgBuffer :: Cfg -> Int
cfgBuffer = buffer

cfgMakeGen :: Cfg -> Int -> IO Gen
cfgMakeGen (Cfg {seed}) tid
  | Just s  <- seed = newGen (s `xor` tid)
  | Nothing <- seed = systemGen

cfgMode :: Cfg -> Mode
cfgMode = mode

readCustomFieldSpec :: ReadM FieldSpec
readCustomFieldSpec = maybeReader reader
  where reader value
          | [rows, columns, mines] <- splitOn "x" value =
              Custom <$> readMaybe rows <*>
                         readMaybe columns <*>
                         readMaybe mines
          | otherwise = Nothing

readFieldSpec :: ReadM FieldSpec
readFieldSpec = Read.oneOf mnemonicSpecs <|> readCustomFieldSpec
  where mnemonicSpecs  = [("easy", Easy), ("medium", Medium), ("hard", Hard)]

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

  where names = presentList $ map name knownPlayers

readStartMove :: ReadM StartMove
readStartMove = Read.oneOf [("center", Center), ("corner", Corner)]

parseStartMove :: Parser StartMove
parseStartMove =
  option readStartMove (long "start-move"
                        <> metavar "START"
                        <> value Center
                        <> showDefault
                        <> help "Start move (center or corner)")

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
  option (Just <$> Read.int) (long "seed"
                              <> metavar "SEED"
                              <> value Nothing
                              <> help "Override default random seed")

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
  where modeUI = command "ui" $ info (ModeUI <$> parseModeUI) uiDesc
        uiDesc = progDesc "View a bot play using Web interface"

        modeBench = command "bench" $
                      info (ModeBench <$> parseModeBench env) benchDesc
        benchDesc = progDesc "Benchmark bot's performance"

cfg :: SystemEnv -> Parser Cfg
cfg env =
  Cfg
  <$> parseFieldSpec
  <*> parsePlayer
  <*> parseStartMove
  <*> parseBufferZone
  <*> parseSeed
  <*> parseMode env

getSystemEnv :: IO (SystemEnv)
getSystemEnv = SystemEnv <$> getNumProcessors

runWithCfg :: (Cfg -> IO ()) -> IO ()
runWithCfg body =
  do env <- getSystemEnv
     let parser = info (helper <*> cfg env) (desc <> fullDesc)

     execParser parser >>= body
  where desc = progDesc "View and benchmark minesweeper bots."
