module CmdArgs
       (
         Cfg
       , Mode(ModeUI)
       , UICfg
       , cfgPlayer
       , cfgStartMove
       , cfgFieldSpec
       , cfgMode
       , cfgBuffer
       , uiInteractive
       , uiDelay
       , runWithCfg
       )
       where

import Data.List (intercalate, find)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)

import Options.Applicative (Parser, ReadM,
                            (<>),
                            execParser,
                            command, subparser,
                            helper, info, fullDesc, progDesc,
                            long, short, metavar, help, value, showDefault,
                            option, flag, eitherReader)

import Player (Player(name))

import qualified Player.Dummy as Dummy
import qualified Player.SinglePoint as SinglePoint

knownPlayers :: [Player]
knownPlayers = [SinglePoint.player, Dummy.player]

defaultPlayer :: Player
defaultPlayer = head knownPlayers

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

data GameCfg =
  GameCfg { field  :: Field
          , player :: Player
          , start  :: StartMove
          , buffer :: Int
          }

data UICfg =
  UICfg { uiInteractive :: Bool
        , uiDelay       :: Int
        }

data Mode = ModeUI UICfg

data Cfg =
  Cfg { cfgGameCfg :: GameCfg
      , cfgMode    :: Mode
      }

cfgFieldSpec :: Cfg -> (Int, Int, Int)
cfgFieldSpec = spec . field . cfgGameCfg
  where spec Easy           = (10, 10, 10)
        spec Medium         = (16, 16, 40)
        spec Hard           = (16, 30, 99)
        spec (Custom r c m) = (r, c, m)

cfgStartMove :: Cfg -> (Int, Int)
cfgStartMove cfg = go (start $ cfgGameCfg cfg)
  where go Corner = (0, 0)
        go Center = (rows `div` 2, columns `div` 2)
          where (rows, columns, _) = cfgFieldSpec cfg

cfgPlayer :: Cfg -> Player
cfgPlayer = player . cfgGameCfg

cfgBuffer :: Cfg -> Int
cfgBuffer = buffer . cfgGameCfg

fieldOpt :: ReadM Field
fieldOpt = eitherReader parse
  where parse "easy"   = Right Easy
        parse "medium" = Right Medium
        parse "hard"   = Right Hard
        parse s        =
          case splitOn "x" s of
           [rows, columns, mines] ->
             maybe err Right (Custom
                              <$> readMaybe rows
                              <*> readMaybe columns
                              <*> readMaybe mines)
           _                      -> err
          where err = Left ("can't understand field description `" ++ s ++ "`")

intOpt :: (Int -> Bool) -> String -> ReadM Int
intOpt pred msg = eitherReader parse
  where parse s = maybe (Left msg) Right (readMaybe s >>= validate)

        validate x | pred x    = Just x
                   | otherwise = Nothing

delayOpt :: ReadM Int
delayOpt = intOpt (>0) "must be a positive integer"

playerOpt :: ReadM Player
playerOpt = eitherReader parse
  where parse s
          | Just player <- find ((== s) . name) knownPlayers = Right player
          | otherwise = Left ("unknown player name `" ++ s ++ "`")

startMoveOpt :: ReadM StartMove
startMoveOpt = eitherReader parse
  where parse "center" = Right Center
        parse "corner" = Right Corner
        parse s        = Left ("can't understand start position `" ++ s ++ "`")

bufferOpt :: ReadM Int
bufferOpt = intOpt (>=0) "must be a non-negative integer"

gameCfg :: Parser GameCfg
gameCfg =
  GameCfg
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
  where names = intercalate ", " (map name knownPlayers)

uiCfg :: Parser UICfg
uiCfg =
  UICfg
  <$> flag True False (long "non-interactive"
                       <> short 'n'
                       <> help "Run in non-interactive mode")
  <*> option delayOpt (long "delay"
                       <> short 'd'
                       <> metavar "DELAY"
                       <> value 200
                       <> showDefault
                       <> help "Delay (in ms) to use in non-interactive mode")

mode :: Parser Mode
mode = subparser modeUI
  where modeUI = command "ui" (info (ModeUI <$> uiCfg) uiDesc)
        uiDesc = progDesc "Run UI"

cfg :: Parser Cfg
cfg = Cfg <$> gameCfg <*> mode

runWithCfg :: (Cfg -> IO ()) -> IO ()
runWithCfg body = execParser (info (helper <*> cfg) fullDesc) >>= body
