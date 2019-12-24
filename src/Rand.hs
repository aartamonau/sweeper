module Rand
  ( Rand
  , MonadRandom
  , Gen
  , newGen
  , systemGen
  , runRand
  , randomSubset
  , uniform
  , uniformR
  ) where

import Control.Monad (forM, when)
import Control.Monad.ST (RealWorld, ST, stToIO, runST)
import qualified Data.Array.MArray as MArr
import Data.Array.ST (STArray)
import Data.Vector (singleton)

import System.Random.MWC (GenST, asGenST, initialize, withSystemRandom)
import System.Random.MWC.Monad (RandST, uniform, uniformR)
import qualified System.Random.MWC.Monad as M

import Control.Monad.Random.Class (MonadRandom, getRandomR)

type Rand a = RandST RealWorld a

newtype Gen =
  Gen
    { unGen :: GenST RealWorld
    }

newGen :: Int -> IO Gen
newGen = stToIO . (fmap Gen) . initialize . singleton . fromIntegral

systemGen :: IO Gen
systemGen = withSystemRandom $ asGenST (return . Gen)

runRand :: Rand a -> Gen -> IO a
runRand r = stToIO . M.runRand r . unGen

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
