module Rand
       (
         Rand
       , Gen
       , newGen
       , runRand

       , uniform
       , uniformR
       ) where

import Control.Monad.ST (RealWorld, stToIO)
import Data.Vector (singleton)

import System.Random.MWC (GenST, initialize)
import System.Random.MWC.Monad (RandST, uniform, uniformR)
import qualified System.Random.MWC.Monad as M

type Rand a = RandST RealWorld a

newtype Gen = Gen { unGen :: GenST RealWorld }

newGen :: Int -> IO Gen
newGen = stToIO . (fmap Gen) . initialize . singleton . fromIntegral

runRand :: Rand a -> Gen -> IO a
runRand r = stToIO . M.runRand r . unGen
