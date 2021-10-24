module Utils.Random (
    MonadRandom (getRandom, getRandomR, getRandomRs, getRandoms),
    StdGen,
    Coin (Heads, Tails),
    coin,
    evalRand,
    mkStdGen,
    newStdGen,
    random,
    randomR,
    randomRs,
    randomSubset,
    randoms,
    split,
    splits,
) where

import Control.Monad (forM, when)
import Control.Monad.Random.Class (
    MonadRandom (getRandom, getRandomR, getRandomRs, getRandoms),
 )
import Control.Monad.Random.Strict (evalRand)
import Control.Monad.ST (ST, runST)
import qualified Data.Array.MArray as MArr
import Data.Array.ST (STArray)
import Data.List (unfoldr)
import System.Random (
    StdGen,
    mkStdGen,
    newStdGen,
    random,
    randomR,
    randomRs,
    randoms,
    split,
 )

randomSubset :: MonadRandom m => Int -> [a] -> m [a]
randomSubset k xs = do
    rs <- forM (zip [k ..] rest) $ \(i, elem) -> do
        (,elem) <$> getRandomR (0, i -1)
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

splits :: StdGen -> [StdGen]
splits = unfoldr (Just . split)

data Coin = Heads | Tails

coin :: MonadRandom m => m Coin
coin =
    getRandomR (0, 1 :: Int) >>= \case
        0 -> return Heads
        _ -> return Tails
