module Utils.Random
  ( MonadRandom(getRandom, getRandomR, getRandomRs, getRandoms)
  , randomSubset
  ) where

import Control.Monad (forM, when)
import Control.Monad.Random.Class
  ( MonadRandom(getRandom, getRandomR, getRandomRs, getRandoms)
  )
import Control.Monad.ST (ST, runST)
import qualified Data.Array.MArray as MArr
import Data.Array.ST (STArray)

randomSubset :: MonadRandom m => Int -> [a] -> m [a]
randomSubset k xs = do
  rs <- forM (zip [k..] rest) $ \(i, elem) -> do
          (,elem) <$> getRandomR (0, i-1)
  return $ runST (go rs)

  where
    (init, rest) = splitAt k xs

    go rs = do
      arr <- MArr.newListArray (0, k - 1) init
      mapM_ (maybeSwap arr) rs
      MArr.getElems arr

    maybeSwap :: STArray s Int a -> (Int, a) -> ST s ()
    maybeSwap arr (r, x) = do
      when (r < k) $ MArr.writeArray arr r x
