{-# LANGUAGE RecordWildCards #-}

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

instance Monoid PlayStats where
  mempty = PlayStats 0 0 0

  (PlayStats xw xl xs) `mappend` (PlayStats yw yl ys) =
    PlayStats (xw + yw) (xl + yl) (xs + ys)

numPlayed :: PlayStats -> Int
numPlayed (PlayStats {..}) = numWon + numLost + numStalled

incWon :: PlayStats -> PlayStats
incWon stats@(PlayStats {..}) = stats { numWon = numWon+1 }

incLost :: PlayStats -> PlayStats
incLost stats@(PlayStats {..}) = stats { numLost = numLost+1 }

incStalled :: PlayStats -> PlayStats
incStalled stats@(PlayStats {..}) = stats { numStalled = numStalled+1 }
