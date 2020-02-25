module GameRunner
  (
    GameResult(GameWon, GameLost)
  , run
  , trace
  ) where

import Control.Monad (when)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.Extra (whenJustM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, asks)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Bifunctor (first)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Tuple (swap)

import Game (Game, Pos)
import qualified Game as Game
import Player
  ( MonadPlayer(getPlayerView, markMine, openEmpty)
  , PlayerL
  , PlayerView
  , makePlayerView
  )
import Utils.Random
  ( MonadRandom(getRandom, getRandomR, getRandomRs, getRandoms)
  , StdGen
  )
import qualified Utils.Random as Random

data GameResult
  = GameWon
  | GameLost

data Env m =
  Env
    { game :: IORef Game
    , gen :: IORef StdGen
    , tracer :: Maybe (Game -> m ())
    }

newtype Runner m a =
  Runner
    { unRunner :: ReaderT (Env m) (ExceptT GameResult m) a
    }

deriving instance Functor m => Functor (Runner m)
deriving instance Monad m => Applicative (Runner m)
deriving instance Monad m => Monad (Runner m)
deriving instance Monad m => MonadReader (Env m) (Runner m)
deriving instance MonadIO m => MonadIO (Runner m)
deriving instance Monad m => MonadError GameResult (Runner m)

instance MonadTrans Runner where
  lift = Runner . lift . lift

readEnv :: MonadIO m => (Env m -> IORef a) -> Runner m a
readEnv accessor = asks accessor >>= liftIO . readIORef

modifyEnv :: MonadIO m => (Env m -> IORef a) -> (a -> (r, a)) -> Runner m r
modifyEnv accessor f = do
  ref <- asks accessor
  liftIO $ do
    value <- readIORef ref
    let (result, newValue) = f value
    writeIORef ref newValue

    return result

modifyGame :: MonadIO m => (Game -> (r, Game)) -> Runner m r
modifyGame f = modifyEnv game f <* notify

instance MonadIO m => MonadRandom (Runner m) where
  getRandom = liftRandom Random.random
  getRandoms = splitRandom Random.randoms
  getRandomR bounds = liftRandom (Random.randomR bounds)
  getRandomRs bounds = splitRandom (Random.randomRs bounds)

splitRandom :: MonadIO m => (StdGen -> a) -> Runner m a
splitRandom f = liftRandom (first f . Random.split)

liftRandom :: MonadIO m => (StdGen -> (a, StdGen)) -> Runner m a
liftRandom = modifyEnv gen

instance MonadIO m => MonadPlayer (Runner m) where
  openEmpty = doOpenEmpty
  markMine = doMarkMine
  getPlayerView = doGetPlayerView

notify :: MonadIO m => Runner m ()
notify =
  whenJustM (asks tracer) $ \f -> do
    game <- readEnv game
    lift $ f game

doOpenEmpty :: MonadIO m => Pos -> Runner m [Pos]
doOpenEmpty p =
  modifyGame open >>= \case
    Left _ -> throwError GameLost
    Right ps -> checkWon >> return ps

  where
    open game = swap (Game.openEmpty game p)

doMarkMine :: MonadIO m => Pos -> Runner m ()
doMarkMine p =
  modifyGame mark >>= \case
    Left _ -> throwError GameLost
    Right () -> checkWon >> return ()

  where
    mark game = swap (Game.markMine game p)

doGetPlayerView :: MonadIO m => Runner m PlayerView
doGetPlayerView = makePlayerView <$> readEnv game

checkWon :: MonadIO m => Runner m ()
checkWon = do
  isWon <- Game.isWon <$> readEnv game
  when isWon (throwError GameWon)

runRunner :: MonadIO m => Env m -> Runner m GameResult -> m GameResult
runRunner env = fmap mergeEither . runExceptT . runReader . unRunner
  where
    runReader = flip runReaderT env
    mergeEither = either id id

run :: MonadIO m => StdGen -> Game -> PlayerL () -> m GameResult
run = run' Nothing

trace ::
     MonadIO m => (Game -> m ()) -> StdGen -> Game -> PlayerL () -> m GameResult
trace tracer = run' (Just tracer)

run' ::
     MonadIO m
  => Maybe (Game -> m ())
  -> StdGen
  -> Game
  -> PlayerL ()
  -> m GameResult
run' tracer gen game player = do
  gameRef <- liftIO $ newIORef game
  genRef <- liftIO $ newIORef gen
  let env = Env {game = gameRef, gen = genRef, tracer}

  runRunner env $ do
    player
    return GameLost
