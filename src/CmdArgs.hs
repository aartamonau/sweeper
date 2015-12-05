module CmdArgs
       (
         Cfg(interactive, delay, player)
       , fieldSpec
       , runWithCfg
       )
       where

import Data.List (intercalate, find)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)

import Options.Applicative (Parser, ReadM,
                            (<>),
                            execParser,
                            helper, info, fullDesc,
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

data Cfg =
  Cfg { field       :: Field
      , interactive :: Bool
      , delay       :: Int
      , player      :: Player
      }
  deriving Show

fieldSpec :: Cfg -> (Int, Int, Int)
fieldSpec = spec . field
  where spec Easy           = (10, 10, 10)
        spec Medium         = (16, 16, 40)
        spec Hard           = (16, 30, 99)
        spec (Custom r c m) = (r, c, m)

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

delayOpt :: ReadM Int
delayOpt = eitherReader parse
  where parse s = maybe err Right (readMaybe s >>= validate)
        err     = Left "delay must be a positive integer"

        validate d
          | d > 0     = Just d
          | otherwise = Nothing

playerOpt :: ReadM Player
playerOpt = eitherReader parse
  where parse s
          | Just player <- find ((== s) . name) knownPlayers = Right player
          | otherwise = Left ("unknown player name `" ++ s ++ "`")

cfgParser :: Parser Cfg
cfgParser =
  Cfg
  <$> option fieldOpt (long "field"
                       <> short 'f'
                       <> metavar "SPEC"
                       <> value Easy
                       <> showDefault
                       <> help "Field specification (easy, medium, hard or RxCxM)")
  <*> flag True False (long "non-interactive"
                       <> short 'n'
                       <> help "Run in non-interactive mode")
  <*> option delayOpt (long "delay"
                       <> short 'd'
                       <> metavar "DELAY"
                       <> value 200
                       <> showDefault
                       <> help "Delay (in ms) to use in non-interactive mode")
  <*> option playerOpt (long "player"
                        <> short 'p'
                        <> metavar "PLAYER"
                        <> value defaultPlayer
                        <> showDefault
                        <> help ("Player (known: " ++ names ++ ")"))
  where names = intercalate ", " (map name knownPlayers)

runWithCfg :: (Cfg -> IO ()) -> IO ()
runWithCfg body = execParser (info (helper <*> cfgParser) fullDesc) >>= body
