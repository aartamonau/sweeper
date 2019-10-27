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
import Data.List (intercalate, lookup)
import Data.List.Split (splitOn)
import Control.Applicative ((<|>))
import GHC.Conc (getNumProcessors)
import Text.Read (readMaybe)

import Options.Applicative (Parser, ReadM,
                            execParser,
                            command, hsubparser,
                            helper, info, progDesc, fullDesc,
                            long, short, metavar, help, value, showDefault,
                            option, flag,
                            maybeReader, eitherReader, auto, readerError)

import Player (Player(name))

import qualified Player.Constraints as Constraints
import qualified Player.Dummy as Dummy
import qualified Player.SinglePoint as SinglePoint

import Rand (Gen, newGen, systemGen)


knownPlayers :: [Player]
knownPlayers = [Constraints.player, Dummy.player, SinglePoint.player]

defaultPlayer :: Player
defaultPlayer = head knownPlayers

threadGen :: Int -> Int -> IO Gen
threadGen seed tid = newGen (seed `xor` tid)

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
      , mkGen     :: Int -> IO Gen
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
cfgMakeGen = mkGen

cfgMode :: Cfg -> Mode
cfgMode = mode

readAnyInt :: ReadM Int
readAnyInt = auto <|> readerError "must be an integer"

intOpt :: (Int -> Bool) -> String -> ReadM Int
intOpt pred msg = eitherReader parse
  where parse s = maybe (Left msg) Right (readMaybe s >>= validate)

        validate x | pred x    = Just x
                   | otherwise = Nothing

posIntOpt :: ReadM Int
posIntOpt = intOpt (>0) "must be a positive integer"

presentOptions :: [String] -> String
presentOptions options = intercalate ", " butLast ++ maybeLast
  where n = length options
        (butLast, last) = splitAt (n-1) options'
        options' = ["`" ++ option ++ "'" | option <- options]

        maybeLast | [x] <- last = " or " ++ x
                  | otherwise   = ""

readOneOf :: [(String, a)] -> ReadM a
readOneOf pairs = eitherReader doRead
  where doRead value
          | Just x <- lookup value pairs = Right x
          | otherwise = Left errorMsg

        errorMsg = "the value must be one of " ++ presentOptions (map fst pairs)

readCustomFieldSpec :: ReadM FieldSpec
readCustomFieldSpec = maybeReader reader
  where reader value
          | [rows, columns, mines] <- splitOn "x" value =
              Custom <$> readMaybe rows <*>
                         readMaybe columns <*>
                         readMaybe mines
          | otherwise = Nothing

readFieldSpec :: ReadM FieldSpec
readFieldSpec = readOneOf mnemonicSpecs <|> readCustomFieldSpec
  where mnemonicSpecs  = [("easy", Easy), ("medium", Medium), ("hard", Hard)]

cfg :: SystemEnv -> Parser Cfg
cfg env =
  Cfg
  <$> option readFieldSpec (long "field"
                            <> short 'f'
                            <> metavar "SPEC"
                            <> value Easy
                            <> showDefault
                            <> help "Field specification (easy, medium, hard or RxCxM)")
  <*> option playerOpt (long "player"
                        <> short 'p'
                        <> metavar "PLAYER"
                        <> value defaultPlayer
                        <> showDefault
                        <> help ("Player (known: " ++ names ++ ")"))
  <*> option startMoveOpt (long "start-move"
                           <> short 's'
                           <> metavar "START"
                           <> value Center
                           <> showDefault
                           <> help "Start move (center or corner)")
  <*> option bufferOpt (long "buffer-zone"
                        <> short 'b'
                        <> metavar "ROWS"
                        <> value 0
                        <> showDefault
                        <> help "Number of empty boxes surrounding start position")
  <*> option (threadGen <$> readAnyInt) (long "seed"
                                         <> metavar "SEED"
                                         <> value (const systemGen)
                                         <> help "Override default random seed")
  <*> hsubparser (modeUI <> modeBench)
  where names = intercalate ", " (map name knownPlayers)

        bufferOpt = intOpt (>=0) "must be a non-negative integer"

        startMoveOpt = readOneOf [("center", Center), ("corner", Corner)]
        playerOpt = readOneOf [(name p, p) | p <- knownPlayers]

        modeUI = command "ui" $ info (ModeUI <$> uiCfg) uiDesc
        uiDesc = progDesc "View a bot play using Web interface"

        modeBench = command "bench" $ info (ModeBench <$> benchCfg env) benchDesc
        benchDesc = progDesc "Benchmark bot's performance"


uiCfg :: Parser UICfg
uiCfg =
  UICfg
  <$> flag True False (long "non-interactive"
                       <> short 'n'
                       <> help "Run in non-interactive mode")
  <*> option posIntOpt (long "delay"
                        <> short 'd'
                        <> metavar "DELAY"
                        <> value 200
                        <> showDefault
                        <> help "Delay (in ms) to use in non-interactive mode")

benchCfg :: SystemEnv -> Parser BenchCfg
benchCfg (SystemEnv {numCPUs}) =
  BenchCfg
  <$> option posIntOpt (long "num-iters"
                        <> short 'n'
                        <> metavar "ITERS"
                        <> value 1000
                        <> showDefault
                        <> help "Number of games to benchmark the bot on")
  <*> option posIntOpt (long "num-workers"
                        <> short 't'
                        <> metavar "WORKERS"
                        <> value numCPUs
                        <> showDefault
                        <> help "Number of workers to run benchmark on")

getSystemEnv :: IO (SystemEnv)
getSystemEnv = SystemEnv <$> getNumProcessors

runWithCfg :: (Cfg -> IO ()) -> IO ()
runWithCfg body =
  do env <- getSystemEnv
     let parser = info (helper <*> cfg env) (desc <> fullDesc)

     execParser parser >>= body
  where desc = progDesc "View and benchmark minesweeper bots."
