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
                            long, short, metavar, help, value,
                            option, eitherReader)

data Field = Easy | Medium | Hard | Custom Int Int Int
           deriving Show

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
                       <> help "field specification (easy, medium, hard)")

runWithCfg :: (Cfg -> IO ()) -> IO ()
runWithCfg body = execParser (info (helper <*> cfgParser) fullDesc) >>= body
