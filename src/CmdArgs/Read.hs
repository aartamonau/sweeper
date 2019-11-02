module CmdArgs.Read
  (
    int
  , nonNegativeInt
  , positiveInt
  ) where

import Control.Applicative ((<|>))
import Options.Applicative (ReadM, auto, readerError)

int :: ReadM Int
int = readInt (const True) "must be an integer"

positiveInt :: ReadM Int
positiveInt = readInt (>0) "must be a positive integer"

nonNegativeInt :: ReadM Int
nonNegativeInt = readInt (>=0) "must be a non-negative integer"

readInt :: (Int -> Bool) -> String -> ReadM Int
readInt pred errorMsg = go <|> readerError errorMsg
  where go = do value <- auto
                if pred value then return value else fail ""
