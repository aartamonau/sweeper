module Rand
       (
         Rand
       , Gen
       , newGen
       , systemGen
       , runRand

       , uniform
       , uniformR
       ) where

import Control.Monad.ST (RealWorld, stToIO)
import Data.Vector (singleton)

import System.Random.MWC (GenST, initialize, withSystemRandom, asGenST)
import System.Random.MWC.Monad (RandST, uniform, uniformR)
import qualified System.Random.MWC.Monad as M

type Rand a = RandST RealWorld a

newtype Gen = Gen { unGen :: GenST RealWorld }

newGen :: Int -> IO Gen
newGen = stToIO . (fmap Gen) . initialize . singleton . fromIntegral

systemGen :: IO Gen
systemGen = withSystemRandom $ asGenST (return . Gen)

runRand :: Rand a -> Gen -> IO a
runRand r = stToIO . M.runRand r . unGen
