module GameRunner
  (
    GameResult(GameWon, GameLost)
  , run
  ) where

import Control.Monad (when)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Random.Class
  ( MonadRandom(getRandom, getRandomR, getRandomRs, getRandoms)
  )
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, asks)
import Data.Bifunctor (first)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Tuple (swap)
import System.Random (StdGen)
import qualified System.Random as Random

import Game (Game, Pos)
import qualified Game as Game
import Player
  ( MonadPlayer(getGame, markMine, openEmpty, posInfo)
  , PlayerL
  )

data GameResult
  = GameWon
  | GameLost

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

readEnv :: (Env -> IORef a) -> Runner a
readEnv accessor = asks accessor >>= liftIO . readIORef

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
  openEmpty = doOpenEmpty
  markMine = doMarkMine
  getGame = doGetGame
  posInfo = doPosInfo

doOpenEmpty :: Pos -> Runner [Pos]
doOpenEmpty p =
  modifyEnv game open >>= \case
    Left _ -> throwError GameLost
    Right ps -> checkWon >> return ps

  where
    open game = swap (Game.openEmpty game p)

doMarkMine :: Pos -> Runner ()
doMarkMine p =
  modifyEnv game mark >>= \case
    Left _ -> throwError GameLost
    Right () -> checkWon >> return ()

  where
    mark game = swap (Game.markMine game p)

doGetGame :: Runner Game
doGetGame = readEnv game

doPosInfo :: [(Pos, String)] -> Runner ()
doPosInfo _ = return ()

checkWon :: Runner ()
checkWon = do
  isWon <- Game.isWon <$> readEnv game
  when isWon (throwError GameWon)

runRunner :: Env -> Runner GameResult -> IO GameResult
runRunner env = fmap mergeEither . runExceptT . runReader . unRunner
  where
    runReader = flip runReaderT env
    mergeEither = either id id

run :: StdGen -> Game -> PlayerL () -> IO GameResult
run gen game player = do
  gameRef <- newIORef game
  genRef <- newIORef gen
  let env = Env {game = gameRef, gen = genRef}

  runRunner env $ do
    player
    return GameLost
