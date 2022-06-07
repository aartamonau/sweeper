module Cli.Read
  ( int,
    nonNegativeInt,
    oneOf,
    positiveInt,
  )
where

import Cli.Helpers (presentList)
import Control.Applicative ((<|>))
import Options.Applicative (ReadM, auto, eitherReader, readerError)

int :: ReadM Int
int = readInt (const True) "must be an integer"

positiveInt :: ReadM Int
positiveInt = readInt (> 0) "must be a positive integer"

nonNegativeInt :: ReadM Int
nonNegativeInt = readInt (>= 0) "must be a non-negative integer"

readInt :: (Int -> Bool) -> String -> ReadM Int
readInt pred errorMsg = go <|> readerError errorMsg
  where
    go = do
      value <- auto
      if pred value
        then return value
        else fail ""

oneOf :: [(String, a)] -> ReadM a
oneOf pairs = eitherReader doRead
  where
    doRead value
      | Just x <- lookup value pairs = Right x
      | otherwise = Left errorMsg
    errorMsg = "the value must be one of " ++ presentList (map fst pairs)
