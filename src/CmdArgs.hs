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
import Data.List (intercalate, find)
import Data.List.Split (splitOn)
import GHC.Conc (getNumProcessors)
import Text.Read (readMaybe)

import Options.Applicative (Parser, ReadM,
                            execParser,
                            command, hsubparser,
                            helper, info, progDesc, fullDesc,
                            long, short, metavar, help, value, showDefault,
                            option, flag, eitherReader)

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

data Field = Easy | Medium | Hard | Custom Int Int Int

instance Show Field where
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
  Cfg { field  :: Field
      , player :: Player
      , start  :: StartMove
      , buffer :: Int
      , mkGen  :: Int -> IO Gen
      , mode   :: Mode
      }

data SystemEnv =
  SystemEnv { numCPUs :: Int }

cfgFieldSpec :: Cfg -> (Int, Int, Int)
cfgFieldSpec = spec . field
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

anyIntOpt :: ReadM Int
anyIntOpt = intOpt (const True) "must be an integer"

intOpt :: (Int -> Bool) -> String -> ReadM Int
intOpt pred msg = eitherReader parse
  where parse s = maybe (Left msg) Right (readMaybe s >>= validate)

        validate x | pred x    = Just x
                   | otherwise = Nothing

posIntOpt :: ReadM Int
posIntOpt = intOpt (>0) "must be a positive integer"

cfg :: SystemEnv -> Parser Cfg
cfg env =
  Cfg
  <$> option fieldOpt (long "field"
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
  <*> option (threadGen <$> anyIntOpt) (long "seed"
                                        <> metavar "SEED"
                                        <> value (const systemGen)
                                        <> help "Override default random seed")
  <*> hsubparser (modeUI <> modeBench)
  where names = intercalate ", " (map name knownPlayers)

        bufferOpt = intOpt (>=0) "must be a non-negative integer"

        startMoveOpt = eitherReader parse
          where parse "center" = Right Center
                parse "corner" = Right Corner
                parse s        = Left ("can't understand start position `"
                                       ++ s ++ "`")

        playerOpt = eitherReader parse
          where parse s
                  | Just player <- find ((== s) . name) knownPlayers =
                      Right player
                  | otherwise =
                      Left ("unknown player name `" ++ s ++ "`")

        fieldOpt = eitherReader parse
          where parse "easy"   = Right Easy
                parse "medium" = Right Medium
                parse "hard"   = Right Hard
                parse s        =
                  case splitOn "x" s of
                   [rows, columns, mines] ->
                     maybe (err s) Right (Custom
                                          <$> readMaybe rows
                                          <*> readMaybe columns
                                          <*> readMaybe mines)
                   _ -> err s

                err s = Left ("can't understand field description `" ++ s ++ "`")

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
