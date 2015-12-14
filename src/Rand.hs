module Rand
       (
         Rand
       , Gen
       , newGen
       , systemGen
       , runRand

       , randomSubset

       , uniform
       , uniformR
       ) where

import Control.Monad (when)
import Control.Monad.ST (ST, RealWorld, stToIO)
import qualified Data.Array.MArray as MArr
import Data.Array.ST (STArray)
import Data.Vector (singleton)

import System.Random.MWC (GenST, initialize, withSystemRandom, asGenST)
import qualified System.Random.MWC as R
import System.Random.MWC.Monad (RandST, uniform, uniformR, toRand)
import qualified System.Random.MWC.Monad as M

type Rand a = RandST RealWorld a

newtype Gen = Gen { unGen :: GenST RealWorld }

newGen :: Int -> IO Gen
newGen = stToIO . (fmap Gen) . initialize . singleton . fromIntegral

systemGen :: IO Gen
systemGen = withSystemRandom $ asGenST (return . Gen)

runRand :: Rand a -> Gen -> IO a
runRand r = stToIO . M.runRand r . unGen

{-# INLINE randomSubset #-}
randomSubset :: Int -> [a] -> Rand [a]
randomSubset k xs
  | length init < k = return xs
  | otherwise       = toRand go
  where (init, rest) = splitAt k xs

        go gen =
          do arr <- MArr.newListArray (0, k-1) init
             mapM_ (maybeSwap gen arr) $ zip [k..] rest
             MArr.getElems arr

        maybeSwap :: GenST RealWorld
                  -> STArray RealWorld Int a -> (Int, a) -> ST RealWorld ()
        maybeSwap gen arr (i, x) =
          do j <- R.uniformR (0, i) gen
             when (j < k) $ MArr.writeArray arr j x
