module GameRunner
  (
  ) where

import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Random.Class
  ( MonadRandom(getRandom, getRandomR, getRandomRs, getRandoms)
  )
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, asks)
import Data.Bifunctor (first)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Random (StdGen)
import qualified System.Random as Random

import Game (Game, Pos)
import Player
  ( MonadPlayer(getGame, markMine, openEmpty, posInfo, surrender)
  , PlayerL
  )

data GameResult = GameAborted

data Env =
  Env
    { game :: IORef Game
    , gen :: IORef StdGen
    }

newtype Runner a =
  Runner
    { unRunner :: ReaderT Env (ExceptT GameResult IO) a
    }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Env
           , MonadIO
           , MonadError GameResult
           )

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
liftRandom = modifyEnv gen

instance MonadPlayer Runner where
  openEmpty p = escape GameAborted
  markMine p = escape GameAborted
  getGame = escape GameAborted
  surrender = escape GameAborted
  posInfo = doPosInfo

doPosInfo :: [(Pos, String)] -> Runner ()
doPosInfo _ = return ()

escape :: GameResult -> Runner a
escape result = throwError result

runRunner :: Env -> Runner GameResult -> IO GameResult
runRunner env = fmap mergeEither . runExceptT . runReader . unRunner
  where
    runReader = flip runReaderT env
    mergeEither = either id id

run :: PlayerL () -> Game -> StdGen -> IO GameResult
run player game gen = do
  gameRef <- newIORef game
  genRef <- newIORef gen
  let env = Env {game = gameRef, gen = genRef}

  runRunner env $ do
    player
    return GameAborted
