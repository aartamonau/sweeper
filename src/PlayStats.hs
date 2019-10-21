module PlayStats
       (
         PlayStats
       , numWon
       , numLost
       , numStalled
       , numPlayed
       , incWon
       , incLost
       , incStalled
       ) where

data PlayStats =
  PlayStats { numWon     :: !Int
            , numLost    :: !Int
            , numStalled :: !Int
            }
  deriving Show

instance Semigroup PlayStats where
  PlayStats xw xl xs <> PlayStats yw yl ys =
    PlayStats (xw + yw) (xl + yl) (xs + ys)

instance Monoid PlayStats where
  mempty = PlayStats 0 0 0

numPlayed :: PlayStats -> Int
numPlayed (PlayStats {numWon, numLost, numStalled}) =
  numWon + numLost + numStalled

incWon :: PlayStats -> PlayStats
incWon stats@(PlayStats {numWon}) = stats { numWon = numWon+1 }

incLost :: PlayStats -> PlayStats
incLost stats@(PlayStats {numLost}) = stats { numLost = numLost+1 }

incStalled :: PlayStats -> PlayStats
incStalled stats@(PlayStats {numStalled}) = stats { numStalled = numStalled+1 }
