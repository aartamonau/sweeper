module GameRunner
  (
  ) where

import Control.Monad.Cont (ContT, MonadCont, runContT, callCC)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Random.Class
  ( MonadRandom(getRandom, getRandomR, getRandomRs, getRandoms)
  )
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, asks)
import Data.Bifunctor (first)
import Data.IORef (IORef, readIORef, writeIORef)
import System.Random (StdGen)
import qualified System.Random as Random

import Game (Game)
import Player
  ( MonadPlayer(getGame, markMine, openEmpty, posInfo, surrender)
  , Player
  )

data GameResult = GameAborted

data Env =
  Env
    { player :: Player
    , game :: IORef Game
    , rndGen :: IORef StdGen

    , escapeCont :: forall a. GameResult -> Runner a
    }

newtype Runner a =
  Runner
    { unRunner :: ContT GameResult (ReaderT Env IO) a
    }
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO, MonadCont)

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

instance MonadPlayer Runner where
  openEmpty p = escape GameAborted
  markMine p = escape GameAborted
  getGame = escape GameAborted
  surrender = escape GameAborted
  posInfo ps = escape GameAborted

escape :: GameResult -> Runner a
escape result = do
  cont <- asks escapeCont
  cont result

runRunner :: Env -> Runner GameResult -> IO GameResult
runRunner env runner = undefined

run :: Player -> Game -> IO GameResult
run = undefined
