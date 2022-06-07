module Stats
  ( Stats,
    numWon,
    numLost,
    numStalled,
    numPlayed,
    update,
  )
where

import GameRunner (GameResult (GameLost, GameWon))

data Stats = Stats
  { numWon :: !Int,
    numLost :: !Int,
    numStalled :: !Int
  }
  deriving (Show)

instance Semigroup Stats where
  Stats xw xl xs <> Stats yw yl ys = Stats (xw + yw) (xl + yl) (xs + ys)

instance Monoid Stats where
  mempty = Stats 0 0 0

numPlayed :: Stats -> Int
numPlayed Stats {numWon, numLost, numStalled} =
  numWon + numLost + numStalled

update :: GameResult -> Stats -> Stats
update GameWon = incWon
update GameLost = incLost

incWon :: Stats -> Stats
incWon stats@Stats {numWon} = stats {numWon = numWon + 1}

incLost :: Stats -> Stats
incLost stats@Stats {numLost} = stats {numLost = numLost + 1}
