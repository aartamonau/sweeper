module GameRunner
  (
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Random.Class
  ( MonadRandom(getRandom, getRandomR, getRandomRs, getRandoms)
  )
import Control.Monad.Reader (ReaderT, MonadReader, asks)
import Data.Bifunctor (first)
import Data.IORef (IORef, readIORef, writeIORef)
import System.Random (StdGen)
import qualified System.Random as Random

import Game (Game)
import Player (Player)

data GameResult

data Env =
  Env
    { player :: Player
    , game :: IORef Game
    , rndGen :: IORef StdGen
    }

newtype Runner a =
  Runner
    { unRunner :: ReaderT Env IO a
    }
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO)

modifyEnv :: (Env -> IORef a) -> (a -> (r, a)) -> Runner r
modifyEnv accessor f = do
  ref <- asks accessor
  liftIO $ do
    value <- readIORef ref
    let (result, newValue) = f value
    writeIORef ref newValue

    return result

instance MonadRandom Runner where
  getRandom = liftRandom Random.random
  getRandoms = splitRandom Random.randoms
  getRandomR bounds = liftRandom (Random.randomR bounds)
  getRandomRs bounds = splitRandom (Random.randomRs bounds)

splitRandom :: (StdGen -> a) -> Runner a
splitRandom f = liftRandom (first f . Random.split)

liftRandom :: (StdGen -> (a, StdGen)) -> Runner a
liftRandom = modifyEnv rndGen

run :: Player -> Game -> IO GameResult
run = undefined
