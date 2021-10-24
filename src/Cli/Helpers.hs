module Cli.Helpers (
    presentList,
) where

import Data.List (intercalate)

presentList :: [String] -> String
presentList options = intercalate ", " butLast ++ maybeLast
  where
    n = length options
    (butLast, last) = splitAt (n - 1) options'
    options' = ["`" ++ option ++ "'" | option <- options]
    maybeLast
        | [x] <- last = " or " ++ x
        | otherwise = ""
