module CmdArgs
       (
         Cfg
       , fieldSpec
       , runWithCfg
       )
       where

import Data.List.Split (splitOn)
import Text.Read (readMaybe)

import Options.Applicative (Parser, ReadM,
                            (<>),
                            execParser,
                            helper, info, fullDesc,
                            long, short, metavar, help, value, showDefault,
                            option, eitherReader)

data Field = Easy | Medium | Hard | Custom Int Int Int

instance Show Field where
  show Easy           = "easy"
  show Medium         = "medium"
  show Hard           = "hard"
  show (Custom r c m) = show r ++ "x" ++ show c ++ "x" ++ show m

data Cfg =
  Cfg { field :: Field }
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

cfgParser :: Parser Cfg
cfgParser =
  Cfg
  <$> option fieldOpt (long "field"
                       <> short 'f'
                       <> metavar "SPEC"
                       <> value Easy
                       <> showDefault
                       <> help "Field specification (easy, medium, hard or RxCxM)")

runWithCfg :: (Cfg -> IO ()) -> IO ()
runWithCfg body = execParser (info (helper <*> cfgParser) fullDesc) >>= body
